module Data.Game.Sprites where

import Prelude ( ($)
               , bind, return )

import Graphics.Canvas ( CanvasImageSource() )
import Optic.Core ( lens )

import Graphics.Canvas.Image ( makeCanvasImageSource )

data Sprites = Sprites
  { player :: CanvasImageSource
  , invader :: Array CanvasImageSource
  }

player = lens (\(Sprites s) -> s.player)
              (\(Sprites s) player' -> Sprites (s { player = player' }))
invader = lens (\(Sprites s) -> s.invader)
               (\(Sprites s) invader' -> Sprites (s { invader = invader' }))

loadSprites = do
  playerSprite <- makeCanvasImageSource "images/player.png"
  invaderSprite1 <- makeCanvasImageSource "images/invader1.png"
  invaderSprite2 <- makeCanvasImageSource "images/invader2.png"

  return $ Sprites
    { player: playerSprite
    , invader: [ invaderSprite1, invaderSprite2 ]
    }
