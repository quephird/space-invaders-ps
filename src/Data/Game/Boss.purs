module Data.Game.Boss where

data Status = Alive
            | Hit
            | Dying
            | Dead

data Boss = Boss
  { x :: Number
  , y :: Number
  , hitsLeft :: Int
  , status :: Status
  }

makeBoss x y = Boss
  { x: x
  , y: y
  , hitsLeft: 25
  , status: Alive
  }
