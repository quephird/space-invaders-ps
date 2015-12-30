module Entities.Player where

import Prelude ( Eq, (#), (==) )

import Data.Time ( Milliseconds(..) )
import Optic.Core ( lens, (.~) )

import Helpers.Lens ( (&) )

data Status = New
            | Alive
            | Dead

instance eqStatus :: Eq Status where
  eq New   New   = true
  eq Alive Alive = true
  eq Dead  Dead  = true
  eq _     _     = false

data Player = Player
  { x :: Number
  , y :: Number
  , startTime :: Milliseconds
  , status :: Status
  }

x = lens (\(Player p) -> p.x)
         (\(Player p) x' -> Player (p { x = x' }))
y = lens (\(Player p) -> p.y)
         (\(Player p) y' -> Player (p { y = y' }))
startTime = lens (\(Player p) -> p.startTime)
                 (\(Player p) startTime' -> Player (p { startTime = startTime' }))
status = lens (\(Player p) -> p.status)
              (\(Player p) status' -> Player (p { status = status' }))

makePlayer x y startTime = Player
  { x: x
  , y: y
  , startTime: startTime
  , status: New
  }

resetPlayer p newX newStartTime = 
  p # x .~ newX
    & status .~ New
    & startTime .~ newStartTime
