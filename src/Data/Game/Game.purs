module Data.Game.Game where

import Prelude ( bind, return, unit
               , ($), (*) )

import Control.Monad.Eff ( Eff() )
import Graphics.Canvas ( Canvas()
                       , CanvasImageSource() )
import Optic.Core ( Lens()
                  , (..)
                  , lens )

import qualified Data.Game.Enemies as E
import qualified Data.Game.Invader as I
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
enemies = lens (\(Game g) -> g.enemies)
               (\(Game g) enemies' -> Game (g { enemies = enemies' }))

playerX :: Lens Game Game Number Number
playerX = player .. P.x
playerY :: Lens Game Game Number Number
playerY = player .. P.y
playerSprite :: Lens Game Game CanvasImageSource CanvasImageSource
playerSprite = sprites .. S.player
invaderSprites :: Lens Game Game (Array CanvasImageSource) (Array CanvasImageSource)
invaderSprites = sprites .. S.invader
invaders :: Lens Game Game (Array I.Invader) (Array I.Invader)
invaders = enemies .. E.invaders

makeGame :: forall eff. Number
         -> Number
         -> Eff ( canvas :: Canvas | eff) Game
makeGame w h = do
  let player = P.makePlayer (0.5*w) (0.9*h)
  sprites <- S.loadSprites
  return $ Game
    { player: player
    , w: w
    , h: h
    , status: Waiting
    , sprites: sprites
    , enemies: E.makeRegularLevel
    }
