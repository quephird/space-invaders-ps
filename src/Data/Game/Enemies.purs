module Data.Game.Enemies where

import Prelude ( ($) )

import Optic.Core ( lens )

import qualified Data.Game.Boss as B
import qualified Data.Game.Invader as I

data Enemies = Patrol (Array I.Invader)
             | BossLevel B.Boss

invaders = lens (\(Patrol invaders) -> invaders)
                (\(Patrol invaders) invaders' -> Patrol invaders')

makeRegularLevel =
  Patrol [ I.makeInvader 100.0 100.0 ]

makeBossLevel =
  BossLevel $ B.makeBoss 600.0 300.0
