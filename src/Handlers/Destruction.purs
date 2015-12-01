module Handlers.Destruction where

import Prelude ( Unit()
               , (#), ($), (<), (>), (+), (/=)
               , bind, otherwise, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( cons, filter )
import Data.Maybe ( Maybe(..) )
import Optic.Core ( (^.), (.~), (+~), (%~) )

import qualified Entities.Bullet as B
import qualified Entities.Event as V
import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as Y
import qualified Entities.Star as T
import Helpers.Lens ( (&) )

removeDeadInvaders :: forall eff g. STRef g G.Game
                   -> Eff ( st :: ST g | eff ) G.Game
removeDeadInvaders gRef = do
  g <- readSTRef gRef
  let invaders = g ^. G.invaders
      newInvaders = filter (\i -> i ^. I.status /= I.Dead) invaders
  modifySTRef gRef (\g -> g # G.invaders .~ newInvaders)

removeDeadMysteryShip :: forall eff g. STRef g G.Game
                      -> Eff ( st :: ST g | eff ) G.Game
removeDeadMysteryShip gRef = do
  g <- readSTRef gRef
  let mysteryShip = g ^. G.mysteryShip
      newMysteryShip (Just m) =
        case (m ^. Y.status) of
          Y.Dead -> Nothing
          _      -> Just m
      newMysteryShip _        = Nothing

      go (Just m) = modifySTRef gRef (\g -> g # G.mysteryShip .~ (newMysteryShip mysteryShip))
      go _        = modifySTRef gRef (\g -> g)

  go mysteryShip


removeOffscreenMysteryShip :: forall eff g. STRef g G.Game
                           -> Eff ( st :: ST g | eff ) G.Game
removeOffscreenMysteryShip gRef = do
  g <- readSTRef gRef
  let currMysteryShip   = g ^. G.mysteryShip
      -- Remove ship and stop sound
      go (Just m) | m^.Y.x > g^.G.w+100.0 = do
        modifySTRef gRef (\g -> g # G.mysteryShip .~ Nothing
                                  & G.events %~ (cons $ V.Event V.GoneMysteryShip V.New))
      -- No op
                  | otherwise = modifySTRef gRef (\g -> g)
      -- No op
      go _                    = modifySTRef gRef (\g -> g)

  go currMysteryShip

removeOffscreenBulletsAndStars :: forall eff g. STRef g G.Game
                               -> Eff ( st :: ST g | eff ) G.Game
removeOffscreenBulletsAndStars gRef = do
  g <- readSTRef gRef
  let stars             = g ^. G.stars
      newStars          = filter (\s -> s ^. T.y > -10.0) stars
      playerBullets     = g ^. G.playerBullets
      newPlayerBullets  = filter (\b -> b ^. B.y > -10.0) playerBullets
      invaderBullets    = g ^. G.invaderBullets
      newInvaderBullets = filter (\b -> b ^. B.y < g^.G.h+10.0) invaderBullets
  modifySTRef gRef (\g -> g # G.playerBullets .~ newPlayerBullets
                            & G.invaderBullets .~ newInvaderBullets
                            & G.stars .~ newStars)

removeAllDeadOrOffscreenObjects :: forall eff g. STRef g G.Game
                                -> Eff ( st :: ST g | eff ) G.Game
removeAllDeadOrOffscreenObjects gRef = do
  foreachE [ removeDeadInvaders
           , removeDeadMysteryShip
           , removeOffscreenBulletsAndStars
           , removeOffscreenMysteryShip
           ] (\f -> do
                      f gRef
                      return unit)
  readSTRef gRef
