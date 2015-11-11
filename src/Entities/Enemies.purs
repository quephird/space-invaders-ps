module Entities.Enemies where

import Prelude ( Eq
               , bind, return
               , ($), (+), (*) )

import Data.Array ( (..) )
import Data.Int ( toNumber )
import Data.Maybe ( Maybe(..) )
import Optic.Core ( lens )

import qualified Entities.Boss as B
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as M

data Direction = Left | Right

instance eqDirection :: Eq Direction where
  eq Left Left   = true
  eq Right Right = true
  eq _ _         = false

data Enemies = Patrol { invaders :: Array I.Invader
                      , direction :: Direction
                      , dx :: Number
                      , mysteryShip :: Maybe M.MysteryShip
                      }
             | BossLevel B.Boss

invaders = lens (\(Patrol p) -> p.invaders)
                (\(Patrol p) invaders' -> Patrol ( p { invaders = invaders' }))
direction = lens (\(Patrol p) -> p.direction)
                 (\(Patrol p) direction' -> Patrol ( p { direction = direction' }))
dx = lens (\(Patrol p) -> p.dx)
          (\(Patrol p) dx' -> Patrol ( p { dx = dx' }))
mysteryShip = lens (\(Patrol p) -> p.mysteryShip)
                   (\(Patrol p) mysteryShip' -> Patrol ( p { mysteryShip = mysteryShip' }))

makeRegularLevel :: Enemies
makeRegularLevel =
  Patrol { invaders: makeInvaders
         , direction: Right
         , dx: 2.0
         , mysteryShip: Nothing
         } where
    makeInvaders = do
      x <- 0 .. 7
      y <- 0 .. 2
      return $ I.makeInvader (50.0 + 75.0*toNumber x)
                             (125.0 + 75.0*toNumber y)
                             (x + 7*y)

makeBossLevel =
  BossLevel $ B.makeBoss 600.0 300.0
