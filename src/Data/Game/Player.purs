module Data.Game.Player where

import Optic.Core ( lens )

data Player = Player
  { x :: Number
  , y :: Number
  }

x = lens (\(Player p) -> p.x)
         (\(Player p) x' -> Player (p { x = x' }))
y = lens (\(Player p) -> p.y)
         (\(Player p) y' -> Player (p { y = y' }))
