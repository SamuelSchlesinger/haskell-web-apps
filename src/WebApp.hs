{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module WebApp
  ( WebApp(..)
  , runApp
  ) where

import qualified Servant (Handler(Handler), Context)
import qualified Servant.Server.Internal.Context as ServantContext
import qualified Servant.Server.Internal.ErrorFormatter as ServantErrorFormatter
import Data.Word (Word16)
import Data.Function ((&))
import Servant (ServerT, ServerError, throwError, serveWithContext, hoistServerWithContext, HasServer)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Except (ExceptT(ExceptT))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import qualified Control.Exception as Exception (Handler)
import Control.Monad ((<=<))
import Control.Exception (catch)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy (Proxy(Proxy))
import qualified Network.Wai.Handler.Warp as Warp (Settings, runSettings)
import Data.Kind (Type)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Network.Wai (Middleware)
import System.Remote.Monitoring (forkServerWith)
import System.Metrics (Store, newStore)
import Servant.Ekg (monitorEndpoints, HasEndpoint)
import Data.Streaming.Network.Internal (HostPreference(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Network.Wai.Middleware.RequestLogger (outputFormat, mkRequestLogger, OutputFormat(CustomOutputFormatWithDetails))
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Data.Default.Class (def)


import Servant (EmptyAPI, emptyServer)
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp

class
  ( HasServer (API app) (ContextTypes app)
  , HasEndpoint (API app)
  , KnownSymbol (ConfigFilePath app)
  , FromJSON (Config app)
  , ToJSON (Config app)
  , Monad (Web app)
  , ServantContext.HasContextEntry
      (ContextTypes app ServantContext..++ ServantErrorFormatter.DefaultErrorFormatters)
      ServantErrorFormatter.ErrorFormatters
  ) => WebApp app where
  -- | The monad we'll run the application in. We define a custom monad so that we can completely
  -- control the instances written for it.
  data Web app a :: Type
  
  -- | The configuration for the application. This is data one can keep in a file to provide to
  -- the application before running it.
  data Config app :: Type

  -- | The warp settings in the config.
  warpSettings :: Config app -> Warp.Settings

  -- | The port to run EKG on.
  ekgPort :: Config app -> Word16

  -- | The default name of the file where the config is kept.
  type ConfigFilePath app :: Symbol

  -- | The global computational state for the application. This is data which is derived from the
  -- configuration, but may contain some actively managed resources such as database connections,
  -- threads, a handle to a service, etc.
  data State app :: Type

  -- | The context which the servant application needs to run the application.
  type ContextTypes app :: [Type]

  -- | Creates the 'Servant.Context' which allows us to run the server.
  createContext :: Web app (Servant.Context (ContextTypes app))

  -- | Creates the 'Middleware' which will be applied to the application.
  createMiddleware :: Web app Middleware
  
  -- | The set of endpoints for the application.
  type API app :: Type

  -- | Builds up the state from the config
  newState :: Config app -> IO (State app)

  -- | The entire config should be retrievable from the state.
  askConfig :: Applicative m => Web app (Config app)

  -- | One must be able to run the Monad in 'IO' given the state.
  runWithState :: State app -> Web app a -> IO a

  -- | The server for the application.
  server :: ServerT (API app) (Web app)

data Env = Dev | Prod

runApp :: forall app. WebApp app => Env -> IO ()
runApp env = do
  let configFilePath = symbolVal (Proxy @(ConfigFilePath app))
  config <- eitherDecodeFileStrict configFilePath >>= onLeft couldn'tDecodeConfig
  state <- newState @app config 
  context <- runWithState state createContext
  store <- newStore
  monitor <- monitorEndpoints (Proxy @(API app)) store
  requestLogger <- mkRequestLogger (def { outputFormat = CustomOutputFormatWithDetails formatAsJSON })
  middleware <- runWithState state createMiddleware
  let
    (middleware . monitor . requestLogger -> application) =
      serveWithContext
        (Proxy @(API app))
        context
        (hoistServerWithContext
          (Proxy @(API app))
          (Proxy @(ContextTypes app))
          (ioToHandler . runWithState @app state)
          (server @app)
        )
    host = Warp.getHost (warpSettings @app config)
  forkServerWith store "localhost" (fromIntegral $ ekgPort config)
  Warp.runSettings (warpSettings @app config) application
  where
    ioToHandler :: IO a -> Servant.Handler a
    ioToHandler io = Servant.Handler . ExceptT $ catch @ServerError (Right <$> io) (pure . Left)

    onLeft :: Monad m => (x -> m y) -> Either x y -> m y
    onLeft f = either f pure

    couldn'tDecodeConfig :: String -> IO a
    couldn'tDecodeConfig err = do
      hPutStrLn stderr $ "Could not decode configuration... Error message: " <> err
      exitFailure

