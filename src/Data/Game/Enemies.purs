module Data.Game.Enemies where

import Prelude ( ($) )

import qualified Data.Game.Boss as B
import qualified Data.Game.Invader as I

data Enemies = RegularLevel (Array I.Invader)
             | BossLevel B.Boss

makeRegularLevel =
  RegularLevel [ I.makeInvader 100.0 100.0 ]

makeBossLevel =
  BossLevel $ B.makeBoss 600.0 300.0
