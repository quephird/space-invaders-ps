module Handlers.Sound where

import Prelude ( Unit(), ($), bind )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , readSTRef )
import Optic.Core ( (^.) )

import qualified Entities.Game as G
import qualified Helpers.Audio as A

playNewPlayerBulletSound :: forall g eff. STRef g G.Game
                         -> Eff ( st :: ST g | eff ) Unit
playNewPlayerBulletSound gRef = do
  g <- readSTRef gRef
  A.playSound $ g ^. G.newPlayerBulletSound

playNewInvaderBulletSound :: forall g eff. STRef g G.Game
                          -> Eff ( st :: ST g | eff ) Unit
playNewInvaderBulletSound gRef = do
  g <- readSTRef gRef
  A.playSound $ g ^. G.newInvaderBulletSound

playInvaderShotSound :: forall g eff. STRef g G.Game
                     -> Eff ( st :: ST g | eff ) Unit
playInvaderShotSound gRef = do
  g <- readSTRef gRef
  A.playSound $ g ^. G.invaderShotSound

playPlayerShotSound :: forall g eff. STRef g G.Game
                    -> Eff ( st :: ST g | eff ) Unit
playPlayerShotSound gRef = do
  g <- readSTRef gRef
  A.playSound $ g ^. G.playerShotSound
