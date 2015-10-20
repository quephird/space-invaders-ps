module Data.Game.Game where

import Prelude ( bind, return, unit
               , ($), (*) )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Time ( Milliseconds() )
import Graphics.Canvas ( Canvas()
                       , CanvasImageSource() )
import Optic.Core ( Lens()
                  , (..), (^.)
                  , lens )

import qualified Data.Game.Bullet as B
import qualified Data.Game.Enemies as E
import qualified Data.Game.Invader as I
import qualified Data.Game.Player as P
import qualified Data.Game.Sprites as S

data Status = GameOver
            | Playing
            | Waiting

data Game = Game
  { w :: Number
  , h :: Number
  , startTime :: Milliseconds
  , status :: Status
  , player :: P.Player
  , playerBullets :: Array B.Bullet
  , sprites :: S.Sprites
  , enemies :: E.Enemies
  }

startTime = lens (\(Game g) -> g.startTime)
                 (\(Game g) startTime' -> Game (g { startTime = startTime' }))
player = lens (\(Game g) -> g.player)
              (\(Game g) player' -> Game (g { player = player' }))
playerBullets = lens (\(Game g) -> g.playerBullets)
                     (\(Game g) playerBullets' -> Game (g { playerBullets = playerBullets' }))
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
         -> Eff ( canvas :: Canvas
                , now :: Now | eff) Game
makeGame w h = do
  let player = P.makePlayer (0.5*w) (0.9*h)
  sprites <- S.loadSprites
  startTime <- nowEpochMilliseconds
  return $ Game
    { w: w
    , h: h
    , startTime: startTime
    , status: Waiting
    , player: player
    , playerBullets: []
    , sprites: sprites
    , enemies: E.makeRegularLevel
    }

-- createPlayerBullet :: forall g eff. STRef g Game
--                    -> Eff ( st :: ST g | eff )
createPlayerBullet gRef = do
  g <- readSTRef gRef
  let newPlayerBullet = B.makePlayerBullet (g ^. playerX) (g ^. playerY)
  modifySTRef gRef (\g -> g)
