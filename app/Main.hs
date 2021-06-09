{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import WebApp
import Data.Function ((&))
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Server.Internal.Context as ServantContext
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant (EmptyAPI, emptyServer, Get, JSON)

data Boring

instance WebApp Boring where
  type API Boring = Get '[JSON] String
  type ConfigFilePath Boring = "boring.json"
  data Config Boring = BoringConfig
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)
  newtype Web Boring a = WebBoring { unWebBoring :: IO a }
    deriving newtype (Functor, Applicative, Monad)
  type ContextTypes Boring = '[]
  data State Boring = BoringState
  warpSettings _ = Warp.defaultSettings & Warp.setPort 8080
  createContext = pure ServantContext.EmptyContext
  createMiddleware = pure id
  newState _ = pure BoringState
  askConfig = pure BoringConfig
  ekgPort _ = 2022
  runWithState _ = unWebBoring
  server = pure "Hello"

main :: IO ()
main = runApp @Boring
