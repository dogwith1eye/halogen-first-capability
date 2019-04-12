module Main where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Equality (class TypeEquals, from)

import Component as Component
import LogMessages (class LogMessages)

data LogLevel = Dev | Prod

type Env = { logLevel :: LogLevel }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel of
      Prod -> pure unit
      _ -> Console.log $ show log

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    logLevel = Dev

    environment :: Env
    environment = { logLevel }

    rootComponent :: H.Component HH.HTML Component.Query Unit Void Aff
    rootComponent = H.hoist (runAppM environment) Component.component

  runUI rootComponent unit body
