module Entities.Bullet where

import Optic.Core ( lens )

data Type = Player | Invader | Boss

data Bullet = Bullet
  { type :: Type
  , x :: Number
  , y :: Number
  }

makeBullet t x y = Bullet
  { type: t
  , x: x
  , y: y
  }
makePlayerBullet = makeBullet Player
makeInvaderBullet = makeBullet Invader
makeBossBullet = makeBullet Boss

x = lens (\(Bullet b) -> b.x)
         (\(Bullet b) x' -> Bullet (b { x = x' }))
y = lens (\(Bullet b) -> b.y)
         (\(Bullet b) y' -> Bullet (b { y = y' }))
