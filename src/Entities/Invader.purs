module Entities.Invader where

import Prelude ( Eq, (==) )

import Optic.Core ( lens, (^.) )

data Status = Alive
            | Shot
            | Dead

instance eqStatus :: Eq Status where
  eq Alive Alive = true
  eq Shot  Shot  = true
  eq Dead  Dead  = true
  eq _     _     = false

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

instance eqInvader :: Eq Invader where
  eq i1 i2 = i1^.idx == i2^.idx

makeInvader x y idx = Invader
  { x: x
  , y: y
  , idx: idx
  , status: Alive
  }
