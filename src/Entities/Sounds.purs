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
  , newMysteryShip   :: A.Sound
  , newMysteryBullet :: A.Sound
  , invaderShot      :: A.Sound
  , playerShot       :: A.Sound
  }

newPlayerBullet = lens (\(Sounds s) -> s.newPlayerBullet)
                       (\(Sounds s) newPlayerBullet' -> Sounds (s { newPlayerBullet = newPlayerBullet' }))
newInvaderBullet = lens (\(Sounds s) -> s.newInvaderBullet)
                        (\(Sounds s) newInvaderBullet' -> Sounds (s { newInvaderBullet = newInvaderBullet' }))
newMysteryShip = lens (\(Sounds s) -> s.newMysteryShip)
                      (\(Sounds s) newMysteryShip' -> Sounds (s { newMysteryShip = newMysteryShip' }))
newMysteryBullet = lens (\(Sounds s) -> s.newMysteryBullet)
                        (\(Sounds s) newMysteryBullet' -> Sounds (s { newMysteryBullet = newMysteryBullet' }))
invaderShot = lens (\(Sounds s) -> s.invaderShot)
                   (\(Sounds s) invaderShot' -> Sounds (s { invaderShot = invaderShot' }))
playerShot = lens (\(Sounds s) -> s.playerShot)
                  (\(Sounds s) playerShot' -> Sounds (s { playerShot = playerShot' }))

-- TODO: Need to figure out how insure that all sounds are loaded
loadAllSounds = do
  newPlayerBulletSound  <- A.loadSound "sounds/newPlayerBullet.wav"
  newInvaderBulletSound <- A.loadSound "sounds/newInvaderBullet.wav"
  newMysteryShipSound   <- A.loadSound "sounds/newMysteryShip.mp3"
  newMysteryBulletSound <- A.loadSound "sounds/newMysteryBullet.wav"
  invaderShotSound      <- A.loadSound "sounds/invaderShot.wav"
  playerShotSound       <- A.loadSound "sounds/playerShot.wav"

  return $ Sounds
    { newPlayerBullet:  newPlayerBulletSound
    , newInvaderBullet: newInvaderBulletSound
    , newMysteryShip:   newMysteryShipSound
    , newMysteryBullet: newMysteryBulletSound
    , invaderShot:      invaderShotSound
    , playerShot:       playerShotSound
    }
