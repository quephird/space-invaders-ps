module Data.Game.Enemies where

import Prelude ( bind, return
               , ($), (+), (*) )

import Data.Array ( (..) )
import Data.Int ( toNumber )
import Optic.Core ( lens )

import qualified Data.Game.Boss as B
import qualified Data.Game.Invader as I

data Enemies = Patrol (Array I.Invader)
             | BossLevel B.Boss

invaders = lens (\(Patrol invaders) -> invaders)
                (\(Patrol invaders) invaders' -> Patrol invaders')

makeRegularLevel :: Enemies
makeRegularLevel =
  Patrol $ do
    x <- 0 .. 7
    y <- 0 .. 2
    return $ I.makeInvader (50.0 + 75.0*toNumber x)
                           (50.0 + 75.0*toNumber y)
                           (x + 7*y)

makeBossLevel =
  BossLevel $ B.makeBoss 600.0 300.0
