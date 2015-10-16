module Data.Game.Invader where

import Optic.Core ( lens )

data Status = Alive
            | Exploding
            | Dead

data Invader = Invader
  { x :: Number
  , y :: Number
  , status :: Status
  }

x = lens (\(Invader i) -> i.x)
         (\(Invader i) x' -> Invader (i { x = x' }))
y = lens (\(Invader i) -> i.y)
         (\(Invader i) y' -> Invader (i { y = y' }))
status = lens (\(Invader i) -> i.status)
              (\(Invader i) status' -> Invader (i { status = status' }))

makeInvader x y = Invader
  { x: x
  , y: y
  , status: Alive
  }
