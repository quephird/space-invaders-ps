module Data.Game.Game where

import Graphics.Canvas ( CanvasImageSource() )
import Optic.Core ( Lens()
                  , (..)
                  , lens )

import qualified Data.Game.Enemies as E
import qualified Data.Game.Player as P
import qualified Data.Game.Sprites as S

data Status = GameOver
            | Playing
            | Waiting

data Game = Game
  { player :: P.Player
  , w :: Number
  , h :: Number
  , status :: Status
  , sprites :: S.Sprites
  , enemies :: E.Enemies
  }

player = lens (\(Game g) -> g.player)
              (\(Game g) player' -> Game (g { player = player' }))
sprites = lens (\(Game g) -> g.sprites)
               (\(Game g) sprites' -> Game (g { sprites = sprites' }))

playerX :: Lens Game Game Number Number
playerX = player .. P.x
playerY :: Lens Game Game Number Number
playerY = player .. P.y
playerSprite :: Lens Game Game CanvasImageSource CanvasImageSource
playerSprite = sprites .. S.player