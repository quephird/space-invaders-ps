module Entities.Bullet where

import Prelude ( Eq, (==), (&&) )

import Optic.Core ( lens, (^.) )

data Type = Player | Invader | Boss

data Bullet = Bullet
  { type :: Type
  , x :: Number
  , y :: Number
  }

instance eqType :: Eq Type where
  eq Player  Player  = true
  eq Invader Invader = true
  eq Boss    Boss    = true
  eq _       _       = false

makeBullet t x y = Bullet
  { type: t
  , x: x
  , y: y
  }
makePlayerBullet = makeBullet Player
makeInvaderBullet = makeBullet Invader
makeBossBullet = makeBullet Boss

bulletType = lens (\(Bullet b) -> b.type)
                  (\(Bullet b) type' -> Bullet (b { type = type' }))
x = lens (\(Bullet b) -> b.x)
         (\(Bullet b) x' -> Bullet (b { x = x' }))
y = lens (\(Bullet b) -> b.y)
         (\(Bullet b) y' -> Bullet (b { y = y' }))

-- TODO: rely on unique ID's instead of coordinates; this is kinda hacky. :S
instance eqBullet :: Eq Bullet where
  eq b1 b2 = b1^.bulletType ==b2^.bulletType &&
             b1^.x == b2^.x &&
             b1^.y == b2^.y
