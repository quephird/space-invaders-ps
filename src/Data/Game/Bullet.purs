module Data.Game.Bullet where

data Type = Player | Invader | Boss

data Bullet = Bullet
  { type :: Type
  , x :: Number
  , y :: Number
  }

makeBullet t x y =
  { type: t
  , x: x
  , y: y
  }
makePlayerBullet = makeBullet Player
makeInvaderBullet = makeBullet Invader
makeBossBullet = makeBullet Boss
