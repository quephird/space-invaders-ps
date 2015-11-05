module Entities.Sounds where

import Prelude ( Unit()
               , ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Optic.Core ( lens )

import qualified Helpers.Audio as A

data Sounds = Sounds
  { newPlayerBullet :: A.Sound
  , invaderShot :: A.Sound
  }

newPlayerBullet = lens (\(Sounds s) -> s.newPlayerBullet)
                       (\(Sounds s) newPlayerBullet' -> Sounds (s { newPlayerBullet = newPlayerBullet' }))
invaderShot = lens (\(Sounds s) -> s.invaderShot)
                   (\(Sounds s) invaderShot' -> Sounds (s { invaderShot = invaderShot' }))

-- TODO: Need to figure out how insure that all sounds are loaded
loadAllSounds = do
  newPlayerBulletSound <- A.loadSound "sounds/newPlayerBullet.wav"
  invaderShotSound     <- A.loadSound "sounds/invaderShot.wav"

  return $ Sounds
    { newPlayerBullet: newPlayerBulletSound
    , invaderShot:     invaderShotSound
    }
