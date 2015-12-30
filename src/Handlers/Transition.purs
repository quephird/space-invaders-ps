module Handlers.Transition where

import Prelude ( Unit()
               , (#), ($), (>=), (-)
               , bind, map, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Maybe ( Maybe(..) )
import Data.Time ( Milliseconds(..) )
import Optic.Core ( (^.), (.~) )

import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Entities.Player as P
import qualified Entities.MysteryShip as Y

-- TODO: Check between now and player start time 
--       if time is greater than three seconds and current status is new
--         then update player status to alive
--         else leave status be
--         
updatePlayerStatus :: forall eff g. STRef g G.Game
                   -> Eff ( now :: Now
                          , st :: ST g | eff ) G.Game
updatePlayerStatus gRef = do
  g <- readSTRef gRef
  currTime <- nowEpochMilliseconds
  let playerLifeTime = currTime - g ^. G.playerStartTime
      go true = modifySTRef gRef (\g -> g # G.playerStatus .~ P.Alive)
      go _    = readSTRef gRef
  go (playerLifeTime >= (Milliseconds 3000.0))

updateInvaderStatus gRef = do
  g <- readSTRef gRef
  let invaders = g ^. G.invaders
      newStatus status =
        case status of
          I.Shot -> I.Dead
          _       -> status
      newInvaders = map (\i -> i # I.status .~ (newStatus $ i^.I.status)) invaders
  modifySTRef gRef (\g -> g # G.invaders .~ newInvaders)

updateMysteryShipStatus gRef = do
  g <- readSTRef gRef
  let mysteryShip = g ^. G.mysteryShip
      newMysteryShip (Just m) =
        case (m ^. Y.status) of
          Y.Shot -> Just $ m # Y.status .~ Y.Dead
          _      -> Just m
      newMysteryShip _        = Nothing

      go (Just m) = modifySTRef gRef (\g -> g # G.mysteryShip .~ (newMysteryShip mysteryShip))
      go _        = modifySTRef gRef (\g -> g)

  go mysteryShip

updateAllStatuses :: forall eff g. STRef g G.Game
                  -> Eff ( now :: Now
                         , st :: ST g | eff ) G.Game
updateAllStatuses gRef = do
  foreachE [ updatePlayerStatus
           , updateInvaderStatus
           , updateMysteryShipStatus
           ] (\f -> do
                      f gRef
                      return unit)
  readSTRef gRef
