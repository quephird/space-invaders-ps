module Data.Game.Invader where

import Optic.Core ( lens )

data Status = Alive
            | Exploding
            | Dead

data Invader = Invader
  { x :: Number
  , y :: Number
  , idx :: Int
  , status :: Status
  }

x = lens (\(Invader i) -> i.x)
         (\(Invader i) x' -> Invader (i { x = x' }))
y = lens (\(Invader i) -> i.y)
         (\(Invader i) y' -> Invader (i { y = y' }))
idx = lens (\(Invader i) -> i.idx)
           (\(Invader i) idx' -> Invader (i { idx = idx' }))
status = lens (\(Invader i) -> i.status)
              (\(Invader i) status' -> Invader (i { status = status' }))

makeInvader x y idx = Invader
  { x: x
  , y: y
  , idx: idx
  , status: Alive
  }
