module Update where

import Prelude ( Unit()
               , (#), ($), (==)
               , bind, otherwise, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.Eff.Random ( RANDOM() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( length )
import Data.Date ( Now() )
import Optic.Core ( (^.), (.~) )

import qualified Entities.Enemies as E
import qualified Entities.Game as G
import qualified Handlers.Collision as C
import qualified Handlers.Destruction as D
import qualified Handlers.Generation as N
import qualified Handlers.Motion as M
import qualified Handlers.Transition as R

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

update' G.Playing gRef = do
  foreachE [ checkPlayerDead
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
