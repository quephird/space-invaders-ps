module Entities.MysteryShip where

import Optic.Core ( lens )

data Status = Alive
            | Dying
            | Dead

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
