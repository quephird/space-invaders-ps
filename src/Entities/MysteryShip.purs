module Entities.MysteryShip where

import Prelude ( Eq, (==) )

import Optic.Core ( lens )

data Status = Alive
            | Shot
            | Dead

instance eqStatus :: Eq Status where
  eq Alive Alive = true
  eq Shot  Shot  = true
  eq Dead  Dead  = true
  eq _     _     = false

data MysteryShip = MysteryShip
  { x :: Number
  , y :: Number
  , status :: Status
  }

x = lens (\(MysteryShip i) -> i.x)
         (\(MysteryShip i) x' -> MysteryShip (i { x = x' }))
y = lens (\(MysteryShip i) -> i.y)
         (\(MysteryShip i) y' -> MysteryShip (i { y = y' }))
status = lens (\(MysteryShip i) -> i.status)
              (\(MysteryShip i) status' -> MysteryShip (i { status = status' }))

makeMysteryShip x y = MysteryShip
  { x: x
  , y: y
  , status: Alive
  }
