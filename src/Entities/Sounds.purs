module Entities.Sounds where

import Prelude ( Unit()
               , ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Optic.Core ( lens )

import qualified Helpers.Audio as A

data Sounds = Sounds
  { newPlayerBullet  :: A.Sound
  , newInvaderBullet :: A.Sound
  , invaderShot      :: A.Sound
  }

newPlayerBullet = lens (\(Sounds s) -> s.newPlayerBullet)
                       (\(Sounds s) newPlayerBullet' -> Sounds (s { newPlayerBullet = newPlayerBullet' }))
newInvaderBullet = lens (\(Sounds s) -> s.newInvaderBullet)
                        (\(Sounds s) newInvaderBullet' -> Sounds (s { newInvaderBullet = newInvaderBullet' }))
invaderShot = lens (\(Sounds s) -> s.invaderShot)
                   (\(Sounds s) invaderShot' -> Sounds (s { invaderShot = invaderShot' }))

-- TODO: Need to figure out how insure that all sounds are loaded
loadAllSounds = do
  newPlayerBulletSound  <- A.loadSound "sounds/newPlayerBullet.wav"
  newInvaderBulletSound <- A.loadSound "sounds/newInvaderBullet.wav"
  invaderShotSound      <- A.loadSound "sounds/invaderShot.wav"

  return $ Sounds
    { newPlayerBullet:  newPlayerBulletSound
    , newInvaderBullet: newInvaderBulletSound
    , invaderShot:      invaderShotSound
    }
