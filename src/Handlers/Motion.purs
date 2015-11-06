module Handlers.Motion where

import Prelude ( Ordering(..), Unit()
               , ($), (#), (<$>), (<), (>), (+), (-), (*), (/), (||), (==), (<=)
               , bind, compare, flip, map, negate, otherwise, return )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( (!!), findLastIndex, head, length, tail )
import Data.Foldable ( foldl )
import Data.Int ( toNumber )
import Data.Maybe ( Maybe(..) )
import Data.Maybe.Unsafe ( fromJust )
import Math ( max, min )
import Optic.Core ( (^.), (.~), (-~), (+~) )

import qualified Entities.Bullet as B
import qualified Entities.Enemies as E
import qualified Entities.Game as G
import qualified Entities.Invader as I
import Helpers.Lens ( (&) )

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

movePlayerBullets :: forall g eff. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
movePlayerBullets gRef = do
  g <- readSTRef gRef
  let playerBullets = g ^. G.playerBullets
      newPlayerBullets = map (\b -> b # B.y -~ 20.0) playerBullets
  modifySTRef gRef (\g -> g # G.playerBullets .~ newPlayerBullets)

moveInvaderBullets :: forall g eff. STRef g G.Game
                   -> Eff ( st :: ST g | eff ) G.Game
moveInvaderBullets gRef = do
  g <- readSTRef gRef
  let invaderBullets = g ^. G.invaderBullets
      newInvaderBullets = map (\b -> b # B.y +~ 20.0) invaderBullets
  modifySTRef gRef (\g -> g # G.invaderBullets .~ newInvaderBullets)

isPastMargins :: Number -> Number -> Number -> Boolean
isPastMargins minX maxX w = (50.0>minX) || (w-50.0)<maxX

computeDirection :: E.Direction
                 -> Number
                 -> Array I.Invader
                 -> E.Direction
computeDirection currDir _ [] = currDir
computeDirection currDir w invaders =
  let h = fromJust $ head invaders
      t = fromJust $ tail invaders
      minX = foldl (\a i -> min a (i ^. I.x)) (h ^. I.x) t
      maxX = foldl (\a i -> max a (i ^. I.x)) (h ^. I.x) t
      pastMargins = isPastMargins minX maxX w
  in go currDir pastMargins where
    go E.Left true  = E.Right
    go E.Right true = E.Left
    go currDir _    = currDir

computeDx :: E.Direction
          -> E.Direction
          -> Number
          -> Int
          -> Number
computeDx newDir currDir currDx invaderCount =
  let ns    = [1,2,3,4,6,8,12]
      idx   = fromJust $ findLastIndex (\n -> n <= invaderCount) ns
  in toNumber $ 24 / (fromJust $ ns !! idx) * (sign newDir) where
    sign E.Left = -1
    sign E.Right = 1

computeDy :: E.Direction
          -> E.Direction
          -> Number
computeDy newDir currDir | newDir == currDir = 0.0
                         | otherwise         = 32.0

moveInvader :: I.Invader
            -> Number
            -> Number
            -> I.Invader
moveInvader invader newDx newDy = invader # I.x +~ newDx
                                          & I.y +~ newDy

movePatrol :: forall g eff. STRef g G.Game
           -> Eff ( st :: ST g | eff ) G.Game
movePatrol gRef = do
  g <- readSTRef gRef
  let w            = g ^. G.w
      currDir      = g ^. G.patrolDirection
      currDx       = g ^. G.patrolDx
      currInvaders = g ^. G.invaders

      newDir       = computeDirection currDir w currInvaders
      newDx        = computeDx newDir currDir currDx $ length currInvaders
      newDy        = computeDy newDir currDir
      newInvaders  = map (\i -> moveInvader i newDx newDy) currInvaders
  modifySTRef gRef (\g -> g # G.invaders .~ newInvaders
                            & G.patrolDirection .~ newDir
                            & G.patrolDx .~ newDx)
