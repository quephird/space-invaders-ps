module Update where

import Prelude ( Unit()
               , (#), ($), (==), (>=), (*)
               , bind, otherwise, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.Eff.Random ( RANDOM() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( cons, head, length, tail )
import Data.Date ( Now() )
import Data.Foldable ( foldl )
import Data.Maybe.Unsafe ( fromJust )
import Math ( max )
import Optic.Core ( (^.), (.~), (%~) )

import qualified Entities.Enemies as E
import qualified Entities.Event as V
import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Handlers.Collision as C
import qualified Handlers.Destruction as D
import qualified Handlers.Generation as N
import qualified Handlers.Motion as M
import qualified Handlers.Transition as R
import Helpers.Lens ( (&) )

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

checkPlayerDead :: forall eff g. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
checkPlayerDead gRef = do
  g <- readSTRef gRef
  let lives = g ^. G.lives
      go lives | lives == 0 = modifySTRef gRef (\g -> g # G.status .~ G.GameOver)
               | otherwise  = modifySTRef gRef (\g -> g)
  go lives

checkInvadersCleared :: forall eff g. STRef g G.Game
                     -> Eff ( st :: ST g | eff ) G.Game
checkInvadersCleared gRef = do
  g <- readSTRef gRef
  let invaders = g ^. G.invaders
      go 0 = modifySTRef gRef (\g -> g # G.invaders .~ E.makeNewInvaders)
      go _ = modifySTRef gRef (\g -> g)
  go $ length invaders

  
checkInvadersLanded :: forall eff g. STRef g G.Game
                    -> Eff ( st :: ST g | eff ) G.Game
checkInvadersLanded gRef = do
 -- get set of current invaders
  g <- readSTRef gRef
  let invaders = g ^. G.invaders
      h        = g ^. G.h
      firstI   = fromJust $ head invaders
      restI    = fromJust $ tail invaders
      maxY     = foldl (\a i -> max a (i ^. I.y)) (firstI ^. I.y) restI
      hasLanded = maxY >= 0.9*h

      go true = modifySTRef gRef (\g -> g # G.status .~ G.GameOver
                                          & G.events %~ cons (V.Event V.InvadersLanded V.New))
      go _    = modifySTRef gRef (\g -> g)

  go hasLanded
 -- get the maxiumum value
 -- see if it's more than critical value
 --   true -> game over
 --   false -> continue

update' G.Playing gRef = do
  foreachE [ checkInvadersLanded
           , checkPlayerDead
           , checkInvadersCleared
           , D.removeAllDeadOrOffscreenObjects
           , C.checkForAllCollisions
           , M.moveEverything
           , N.generateEverything
           , R.updateAllStatuses
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
