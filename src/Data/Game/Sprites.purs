module Data.Game.Sprites where

import Optic.Core ( lens )

import Graphics.Canvas ( CanvasImageSource() )

data Sprites = Sprites
  { player :: CanvasImageSource
  , invader :: Array CanvasImageSource
  }

player = lens (\(Sprites s) -> s.player)
              (\(Sprites s) player' -> Sprites (s { player = player' }))
