module Handlers.Event where

import Prelude ( Unit()
               , (#), ($), (==), (/=)
               , bind, map, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( filter )
import Optic.Core ( (.~), (^.) )

import qualified Entities.Event as V
import qualified Entities.Game as G
import qualified Handlers.Sound as S

processEvent (V.Event V.InvaderShot _) gRef = do
  S.playInvaderShotSound gRef
processEvent (V.Event V.NewInvaderBullet _) gRef = do
  S.playNewInvaderBulletSound gRef
processEvent _ _ = do
  return unit

clearHandledEvents gRef = do
  g <- readSTRef gRef
  let currEvents = g ^. G.events
      unhandledEvents = filter (\e -> e ^. V.status == V.New) currEvents
  modifySTRef gRef (\g -> g # G.events .~ unhandledEvents)

processEvents :: forall eff g. STRef g G.Game
              -> Eff ( st :: ST g | eff ) Unit
processEvents gRef = do
  g <- readSTRef gRef
  let currEvents = g ^. G.events
      unhandledEvents  = filter (\e -> e ^. V.status == V.New) currEvents
      newEvents = map (\e -> e # V.status .~ V.Handled) currEvents
  foreachE unhandledEvents $ \event -> do
    processEvent event gRef
    return unit
  modifySTRef gRef (\g -> g # G.events .~ newEvents)
  return unit
