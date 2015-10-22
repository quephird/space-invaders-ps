module Entities.Player where

import Optic.Core ( lens )

data Status = Alive | Dead

data Player = Player
  { x :: Number
  , y :: Number
  , status :: Status
  }

x = lens (\(Player p) -> p.x)
         (\(Player p) x' -> Player (p { x = x' }))
y = lens (\(Player p) -> p.y)
         (\(Player p) y' -> Player (p { y = y' }))
status = lens (\(Player p) -> p.status)
              (\(Player p) status' -> Player (p { status = status' }))

makePlayer x y = Player
  { x: x
  , y: y
  , status: Alive
  }
