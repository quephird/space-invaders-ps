module Entities.Enemies where

import Prelude ( bind, return
               , ($), (+), (*) )

import Data.Array ( (..) )
import Data.Int ( toNumber )
import Optic.Core ( lens )

import qualified Entities.Boss as B
import qualified Entities.Invader as I

data Direction = Left | Right

data Enemies = Patrol (Array I.Invader) Direction Number
             | BossLevel B.Boss

invaders = lens (\(Patrol invaders _ _) -> invaders)
                (\(Patrol invaders dir dx) invaders' -> Patrol invaders' dir dx)
direction = lens (\(Patrol _ dir _) -> dir)
                 (\(Patrol invaders dir dx) dir' -> Patrol invaders dir' dx)
dx = lens (\(Patrol _ _ dx) -> dx)
          (\(Patrol invaders dir dx) dx' -> Patrol invaders dir dx')

makeRegularLevel :: Enemies
makeRegularLevel =
  Patrol makeInvaders Right 5.0 where
    makeInvaders = do
      x <- 0 .. 7
      y <- 0 .. 2
      return $ I.makeInvader (50.0 + 75.0*toNumber x)
                             (50.0 + 75.0*toNumber y)
                             (x + 7*y)

makeBossLevel =
  BossLevel $ B.makeBoss 600.0 300.0
