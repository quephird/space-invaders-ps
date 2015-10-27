module Motion where

import Prelude ( Ordering(..), Unit()
               , ($), (#), (<$>), (<), (>), (-), (||), (==)
               , bind, compare, flip, map, negate, otherwise, return )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( head, tail )
import Data.Foldable ( foldl )
import Data.Maybe ( Maybe(..) )
import Data.Maybe.Unsafe ( fromJust )
import Math ( max, min )
import Optic.Core ( (^.), (.~), (-~), (+~) )

import qualified Entities.Bullet as B
import qualified Entities.Enemies as E
import qualified Entities.Game as G
import qualified Entities.Invader as I

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

movePlayerBullets :: forall g eff. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
movePlayerBullets gRef = do
  g <- readSTRef gRef
  let playerBullets = g ^. G.playerBullets
      newPlayerBullets = map (\b -> b # B.y -~ 20.0) playerBullets
  modifySTRef gRef (\g -> g # G.playerBullets .~ newPlayerBullets)

maxArray :: Array Number -> Maybe Number
maxArray [] = Nothing
maxArray as = do
  h <- head as
  t <- tail as
  return $ foldl max h t

maxInvaderX :: Array I.Invader
            -> Maybe Number
maxInvaderX [] = Nothing
maxInvaderX is = do
  h <- head is
  t <- tail is
  return $ foldl (\a i -> max a (i ^. I.x)) (h ^. I.x) t

minInvaderX :: Array I.Invader
            -> Maybe Number
minInvaderX [] = Nothing
minInvaderX is = do
  h <- head is
  t <- tail is
  return $ foldl (\a i -> min a (i ^. I.x)) (h ^. I.x) t

isPastMargins :: Number -> Number -> Number -> Boolean
isPastMargins minX maxX w = (50.0>minX) || (w-50.0)<maxX

computeDirection :: E.Direction
                 -> Number
                 -> Array I.Invader
                 -> E.Direction
computeDirection currDir _ [] = currDir
computeDirection currDir w invaders =
  let minX = fromJust $ minInvaderX invaders
      maxX = fromJust $ maxInvaderX invaders
      pastMargins = isPastMargins minX maxX w
  in go currDir pastMargins where
    go E.Left true  = E.Right
    go E.Right true = E.Left
    go currDir _    = currDir

computeDx :: E.Direction
          -> E.Direction
          -> Number
          -> Number
computeDx newDir currDir currDx | newDir == currDir = currDx
                                | otherwise         = negate currDx

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

-- This combinator is used below to allow for chaining
-- of lens operations. The implementation was taken directly from here:
--
-- https://hackage.haskell.org/package/lens-4.13/docs/Control-Lens-Lens.html#g:4
(&) :: forall a b. a -> (a -> b) -> b
(&) = flip ($)

movePatrol :: forall g eff. STRef g G.Game
           -> Eff ( st :: ST g | eff ) G.Game
movePatrol gRef = do
  g <- readSTRef gRef
  let w            = g ^. G.w
      currDir      = g ^. G.patrolDirection
      currDx       = g ^. G.patrolDx
      currInvaders = g ^. G.invaders

      minX         = fromJust $ minInvaderX currInvaders
      maxX         = fromJust $ maxInvaderX currInvaders
      pastMargins  = isPastMargins minX maxX w

      newDir       = computeDirection currDir w currInvaders
      newDx        = computeDx newDir currDir currDx
      newDy        = computeDy newDir currDir
      newInvaders  = map (\i -> moveInvader i newDx newDy) currInvaders
  modifySTRef gRef (\g -> g # G.invaders .~ newInvaders
                            & G.patrolDirection .~ newDir
                            & G.patrolDx .~ newDx)
