module Update where

import Prelude ( Unit()
               , (#), ($), (<), (>), (==), (/=), (+)
               , bind, map, otherwise, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.Eff.Random ( RANDOM() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( cons, filter, length )
import Data.Date ( Now() )
import Data.Maybe ( Maybe(..) )
import Optic.Core ( (^.), (.~), (+~), (%~) )

import qualified Entities.Bullet as B
import qualified Entities.Enemies as E
import qualified Entities.Event as V
import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Entities.Star as T
import qualified Entities.MysteryShip as Y
import qualified Handlers.Collision as C
import qualified Handlers.Generation as N
import qualified Handlers.Motion as M
import Helpers.Lens ( (&) )

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

updateInvaderStatus gRef = do
  g <- readSTRef gRef
  let invaders = g ^. G.invaders
      remainingInvaders = filter (\i -> i ^. I.status /= I.Dead) invaders
      newStatus status =
        case status of
          I.Shot -> I.Dead
          _       -> status
      newInvaders = map (\i -> i # I.status .~ (newStatus $ i^.I.status)) remainingInvaders
  modifySTRef gRef (\g -> g # G.invaders .~ newInvaders)

updateMysteryShipStatus gRef = do
  g <- readSTRef gRef
  let mysteryShip = g ^. G.mysteryShip
      newMysteryShip (Just m) =
        case (m ^. Y.status) of
          Y.Shot -> Just $ m # Y.status .~ Y.Dead
          Y.Dead -> Nothing
          _      -> Just m
      newMysteryShip _        = Nothing

      go (Just m) = modifySTRef gRef (\g -> g # G.mysteryShip .~ (newMysteryShip mysteryShip))
      go _        = modifySTRef gRef (\g -> g)

  go mysteryShip

-- updatePatrol gRef = do
--   updateInvaders gRef
--   updateMysteryShip gRef

possiblyRemoveOffscreenMysteryShip gRef = do
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

-- TODO: Think about moving this into a different Module
--         and moving update back into Main.
removeOffscreenObjects :: forall eff g. STRef g G.Game
                       -> Eff ( random :: RANDOM
                              , st :: ST g | eff ) G.Game
removeOffscreenObjects gRef = do
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

checkPlayerDead :: forall eff g. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
checkPlayerDead gRef = do
  g <- readSTRef gRef
  let lives = g ^. G.lives
      go lives | lives == 0 = modifySTRef gRef (\g -> g # G.status .~ G.GameOver)
               | otherwise  = modifySTRef gRef (\g -> g)
  go lives

-- TODO: don't just make new level; a mystery ship may already be in flight
--         instead just create new fleet of invaders
checkInvadersCleared :: forall eff g. STRef g G.Game
                     -> Eff ( st :: ST g | eff ) G.Game
checkInvadersCleared gRef = do
  g <- readSTRef gRef
  let invaders = g ^. G.invaders
      go 0 = modifySTRef gRef (\g -> g # G.enemies .~ E.makeRegularLevel)
      go _ = modifySTRef gRef (\g -> g)
  go $ length invaders

-- TODO: This is still too noisy; need to group function calls better.
update' G.Playing gRef = do
  foreachE [ checkPlayerDead
           , C.checkForAllCollisions
           , checkInvadersCleared
           , M.moveEverything
           , updateInvaderStatus
           , updateMysteryShipStatus
           , N.generateEverything
           , removeOffscreenObjects
           , possiblyRemoveOffscreenMysteryShip
           ] (\f -> do
                      f gRef
                      return unit)
  return unit

update' G.Waiting gRef = do
  return unit

update' G.GameOver gRef = do
  return unit

update :: forall eff g. STRef g G.Game
       -> Eff ( now :: Now
              , random :: RANDOM
              , st :: ST g
              -- , console :: CONSOLE
              | eff ) Unit
update gRef = do
  g <- readSTRef gRef
  let gameStatus = g ^. G.status
  update' gameStatus gRef
