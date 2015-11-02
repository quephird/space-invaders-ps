module Entities.Sounds where

import Prelude ( Unit()
               , ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Optic.Core ( lens )

import qualified Helpers.Audio as A

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
