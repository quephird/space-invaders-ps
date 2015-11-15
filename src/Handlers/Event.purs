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
import qualified Helpers.Audio as A

processEvent (V.Event V.PlayerShot _) g = do
  A.playSound $ g ^. G.playerShotSound
processEvent (V.Event V.InvaderShot _) g = do
  A.playSound $ g ^. G.invaderShotSound
processEvent (V.Event V.NewInvaderBullet _) g = do
  A.playSound $ g ^. G.newInvaderBulletSound
processEvent (V.Event V.NewMysteryShip _) g = do
  A.playSound $ g ^. G.newMysteryShipSound
processEvent (V.Event V.NewMysteryBullet _) g = do
  A.playSound $ g ^. G.newMysteryBulletSound
processEvent (V.Event V.GoneMysteryShip _) g = do
  A.stopSound $ g ^. G.newMysteryShipSound
processEvent _ _ = do
  return unit

clearHandledEvents gRef = do
  g <- readSTRef gRef
  let currEvents = g ^. G.events
      unhandledEvents = filter (\e -> e ^. V.status == V.New) currEvents
  modifySTRef gRef (\g -> g # G.events .~ unhandledEvents)

-- TODO: Need to update the statuses of events once they're processed!!! DUH!!!
processEvents :: forall eff g. STRef g G.Game
              -> Eff ( st :: ST g | eff ) Unit
processEvents gRef = do
  g <- readSTRef gRef
  let currEvents = g ^. G.events
      unhandledEvents  = filter (\e -> e ^. V.status == V.New) currEvents
      newEvents = map (\e -> e # V.status .~ V.Handled) currEvents
  foreachE unhandledEvents $ \event -> do
    processEvent event g
    return unit
  modifySTRef gRef (\g -> g # G.events .~ newEvents)
  return unit
