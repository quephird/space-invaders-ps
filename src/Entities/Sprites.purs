module Entities.Sprites where

import Prelude ( ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Graphics.Canvas ( Canvas(), CanvasImageSource() )
import Optic.Core ( lens )

import Helpers.Image ( makeCanvasImageSource, getWidth )

data Sprites = Sprites
  { player :: CanvasImageSource
  , playerBullet :: CanvasImageSource
  , invader :: Array CanvasImageSource
  , shotInvader :: CanvasImageSource
  , deadInvader :: CanvasImageSource
  , lives :: CanvasImageSource
  }

lives = lens (\(Sprites s) -> s.lives)
             (\(Sprites s) lives' -> Sprites (s { lives = lives' }))
player = lens (\(Sprites s) -> s.player)
              (\(Sprites s) player' -> Sprites (s { player = player' }))
playerBullet = lens (\(Sprites s) -> s.playerBullet)
              (\(Sprites s) playerBullet' -> Sprites (s { playerBullet = playerBullet' }))
invader = lens (\(Sprites s) -> s.invader)
               (\(Sprites s) invader' -> Sprites (s { invader = invader' }))
shotInvader = lens (\(Sprites s) -> s.shotInvader)
                   (\(Sprites s) shotInvader' -> Sprites (s { shotInvader = shotInvader' }))
deadInvader = lens (\(Sprites s) -> s.deadInvader)
                   (\(Sprites s) deadInvader' -> Sprites (s { deadInvader = deadInvader' }))


loadSprites :: forall eff. Eff ( canvas :: Canvas | eff ) Sprites
loadSprites = do
  playerSprite <- makeCanvasImageSource "images/player.png"
  playerBulletSprite <- makeCanvasImageSource "images/playerBullet.png"
  invaderSprite1 <- makeCanvasImageSource "images/invader1.png"
  invaderSprite2 <- makeCanvasImageSource "images/invader2.png"
  shotInvaderSprite <- makeCanvasImageSource "images/invaderShot.png"
  deadInvaderSprite <- makeCanvasImageSource "images/invaderDead.png"
  lifeSprite <- makeCanvasImageSource "images/life.png"

  return $ Sprites
    { player: playerSprite
    , playerBullet: playerBulletSprite
    , invader: [ invaderSprite1, invaderSprite2 ]
    , shotInvader: shotInvaderSprite
    , deadInvader: deadInvaderSprite
    , lives: lifeSprite
    }
