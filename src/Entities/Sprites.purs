module Entities.Sprites where

import Prelude ( ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Graphics.Canvas ( Canvas(), CanvasImageSource() )
import Optic.Core ( lens )

import Graphics.Canvas.Image ( makeCanvasImageSource, getWidth )

data Sprites = Sprites
  { player :: CanvasImageSource
  , playerBullet :: CanvasImageSource
  , invader :: Array CanvasImageSource
  }

player = lens (\(Sprites s) -> s.player)
              (\(Sprites s) player' -> Sprites (s { player = player' }))
playerBullet = lens (\(Sprites s) -> s.playerBullet)
              (\(Sprites s) playerBullet' -> Sprites (s { playerBullet = playerBullet' }))
invader = lens (\(Sprites s) -> s.invader)
               (\(Sprites s) invader' -> Sprites (s { invader = invader' }))

loadSprites :: forall eff. Eff ( canvas :: Canvas | eff ) Sprites
loadSprites = do
  playerSprite <- makeCanvasImageSource "images/player.png"
  playerBulletSprite <- makeCanvasImageSource "images/playerBullet.png"
  invaderSprite1 <- makeCanvasImageSource "images/invader1.png"
  invaderSprite2 <- makeCanvasImageSource "images/invader2.png"

  return $ Sprites
    { player: playerSprite
    , playerBullet: playerBulletSprite
    , invader: [ invaderSprite1, invaderSprite2 ]
    }
