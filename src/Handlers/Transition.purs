module Handlers.Transition where

import Prelude ( Unit()
               , (#), ($)
               , bind, map, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Maybe ( Maybe(..) )
import Optic.Core ( (^.), (.~) )

import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as Y

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
                  -> Eff ( st :: ST g | eff ) G.Game
updateAllStatuses gRef = do
  foreachE [ updateInvaderStatus
           , updateMysteryShipStatus
           ] (\f -> do
                      f gRef
                      return unit)
  readSTRef gRef
