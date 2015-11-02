module Entities.Sounds where

import Prelude ( Unit()
               , ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , readSTRef )
import Optic.Core ( lens )

import qualified Audio as A
-- import qualified Entities.Game as G

data Sounds = Sounds
  { newPlayerBullet :: A.Sound
  }

newPlayerBullet = lens (\(Sounds s) -> s.newPlayerBullet)
                       (\(Sounds s) newPlayerBullet' -> Sounds (s { newPlayerBullet = newPlayerBullet' }))

loadAllSounds = do
  newPlayerBulletSound <- A.loadSound "sounds/newPlayerBullet.wav"

  return $ Sounds
    { newPlayerBullet: newPlayerBulletSound
    }

-- TODO: Need to figure out how to avoid circular dependencies here.
-- playNewPlayerBulletSound :: forall g eff. STRef g G.Game
--                          -> Eff ( st :: ST g | eff ) Unit
-- playNewPlayerBulletSound gRef = do
--   g <- readSTRef gRef
--   A.playSound g.newPlayerBullet
