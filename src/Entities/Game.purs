module Entities.Game where

import Prelude ( (#), ($), (*)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Control.Monad.Eff.Random ( RANDOM() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Maybe ( Maybe(..) )
import Data.Time ( Milliseconds(..) )
import Graphics.Canvas ( Canvas()
                       , CanvasImageSource() )
import Optic.Core ( Lens()
                  , (..), (^.), (.~)
                  , lens )

import qualified Entities.Bullet as B
import qualified Entities.Enemies as E
import qualified Entities.Event as V
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as M
import qualified Entities.Player as P
import qualified Entities.Sounds as O
import qualified Entities.Sprites as S
import qualified Entities.Star as T
import qualified Helpers.Audio as A
import Helpers.Lens ( (&) )

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

data Status = GameOver
            | Playing
            | Waiting

-- TODO: Need to introduce current level property.
data Game = Game
  { w              :: Number
  , h              :: Number
  , frameDuration  :: Milliseconds
  , startTime      :: Milliseconds
  , status         :: Status
  , score          :: Int
  , lives          :: Int
  , player         :: P.Player
  , playerBullet   :: Maybe B.Bullet
  , invaderBullets :: Array B.Bullet
  , enemies        :: E.Enemies
  , events         :: Array V.Event
  , stars          :: Array T.Star
  , sprites        :: S.Sprites
  , sounds         :: O.Sounds
  }

w = lens (\(Game g) -> g.w)
         (\(Game g) w' -> Game (g { w = w' }))
h = lens (\(Game g) -> g.h)
         (\(Game g) h' -> Game (g { h = h' }))
frameDuration = lens (\(Game g) -> g.frameDuration)
                     (\(Game g) frameDuration' -> Game (g { frameDuration = frameDuration' }))
startTime = lens (\(Game g) -> g.startTime)
                 (\(Game g) startTime' -> Game (g { startTime = startTime' }))
status = lens (\(Game g) -> g.status)
              (\(Game g) status' -> Game (g { status = status' }))
score = lens (\(Game g) -> g.score)
             (\(Game g) score' -> Game (g { score = score' }))
lives = lens (\(Game g) -> g.lives)
             (\(Game g) lives' -> Game (g { lives = lives' }))
player = lens (\(Game g) -> g.player)
              (\(Game g) player' -> Game (g { player = player' }))
playerBullet  = lens (\(Game g) -> g.playerBullet )
                     (\(Game g) playerBullet' -> Game (g { playerBullet = playerBullet' }))
invaderBullets = lens (\(Game g) -> g.invaderBullets)
                      (\(Game g) invaderBullets' -> Game (g { invaderBullets = invaderBullets' }))
enemies = lens (\(Game g) -> g.enemies)
               (\(Game g) enemies' -> Game (g { enemies = enemies' }))
events = lens (\(Game g) -> g.events)
              (\(Game g) events' -> Game (g { events = events' }))
stars = lens (\(Game g) -> g.stars)
             (\(Game g) stars' -> Game (g { stars = stars' }))
sprites = lens (\(Game g) -> g.sprites)
               (\(Game g) sprites' -> Game (g { sprites = sprites' }))
sounds = lens (\(Game g) -> g.sounds)
              (\(Game g) sounds' -> Game (g { sounds = sounds' }))

playerX :: Lens Game Game Number Number
playerX = player .. P.x
playerY :: Lens Game Game Number Number
playerY = player .. P.y
invaders :: Lens Game Game (Array I.Invader) (Array I.Invader)
invaders = enemies .. E.invaders
patrolDirection :: Lens Game Game E.Direction E.Direction
patrolDirection = enemies .. E.direction
patrolDx :: Lens Game Game Number Number
patrolDx = enemies .. E.dx
mysteryShip :: Lens Game Game (Maybe M.MysteryShip) (Maybe M.MysteryShip)
mysteryShip = enemies .. E.mysteryShip

lifeSprite :: Lens Game Game CanvasImageSource CanvasImageSource
lifeSprite = sprites .. S.lives
playerSprite :: Lens Game Game CanvasImageSource CanvasImageSource
playerSprite = sprites .. S.player
playerBulletSprite :: Lens Game Game CanvasImageSource CanvasImageSource
playerBulletSprite = sprites .. S.playerBullet
invaderSprites :: Lens Game Game (Array CanvasImageSource) (Array CanvasImageSource)
invaderSprites = sprites .. S.invader
invaderBulletSprite :: Lens Game Game CanvasImageSource CanvasImageSource
invaderBulletSprite = sprites .. S.invaderBullet
shotInvaderSprite :: Lens Game Game CanvasImageSource CanvasImageSource
shotInvaderSprite = sprites .. S.shotInvader
deadInvaderSprite :: Lens Game Game CanvasImageSource CanvasImageSource
deadInvaderSprite = sprites .. S.deadInvader
mysteryShipSprites :: Lens Game Game (Array CanvasImageSource) (Array CanvasImageSource)
mysteryShipSprites = sprites .. S.mysteryShip
mysteryBulletSprite :: Lens Game Game CanvasImageSource CanvasImageSource
mysteryBulletSprite = sprites .. S.mysteryBullet

newPlayerBulletSound :: Lens Game Game A.Sound A.Sound
newPlayerBulletSound = sounds .. O.newPlayerBullet
newInvaderBulletSound :: Lens Game Game A.Sound A.Sound
newInvaderBulletSound = sounds .. O.newInvaderBullet
newMysteryShipSound :: Lens Game Game A.Sound A.Sound
newMysteryShipSound = sounds .. O.newMysteryShip
newMysteryBulletSound :: Lens Game Game A.Sound A.Sound
newMysteryBulletSound = sounds .. O.newMysteryBullet
invaderShotSound :: Lens Game Game A.Sound A.Sound
invaderShotSound = sounds .. O.invaderShot
mysteryShipShotSound :: Lens Game Game A.Sound A.Sound
mysteryShipShotSound = sounds .. O.mysteryShipShot
playerShotSound :: Lens Game Game A.Sound A.Sound
playerShotSound = sounds .. O.playerShot
invadersLandedSound :: Lens Game Game A.Sound A.Sound
invadersLandedSound = sounds .. O.invadersLanded

makeGame :: forall eff. Number
         -> Number
         -> Milliseconds
         -> Eff ( canvas :: Canvas
                , now :: Now
                , random :: RANDOM | eff ) Game
makeGame w h frameDuration = do
  let player = P.makePlayer (0.5*w) (0.9*h)
  sprites <- S.loadSprites
  sounds <- O.loadAllSounds
  startTime <- nowEpochMilliseconds
  stars <- T.generateStars w h
  return $ Game
    { w:              w
    , h:              h
    , frameDuration:  frameDuration
    , startTime:      startTime
    , status:         Waiting
    , score:          0
    , lives:          3
    , player:         player
    , playerBullet:   Nothing
    , invaderBullets: []
    , enemies:        E.makeRegularLevel
    , events:         []
    , stars:          stars
    , sprites:        sprites
    , sounds:         sounds
    }

startGame :: forall g eff. STRef g Game
          -> Eff ( now :: Now
                 , st :: ST g | eff ) Game
startGame gRef = do
  g <- readSTRef gRef
  newStartTime <- nowEpochMilliseconds
  modifySTRef gRef (\g -> g # status .~ Playing
                            & startTime .~ newStartTime)

restartGame :: forall g eff. STRef g Game
            -> Eff ( now :: Now
                   , st :: ST g | eff ) Game
restartGame gRef = do
  g <- readSTRef gRef
  newStartTime <- nowEpochMilliseconds
  let newPlayer = P.makePlayer (0.5*g^.w) (0.9*g^.h)
  modifySTRef gRef (\g -> g # status .~ Playing
                            & player .~ newPlayer
                            & startTime .~ newStartTime
                            & score .~ 0
                            & lives .~ 3
                            & enemies .~ E.makeRegularLevel
                            & playerBullet .~ Nothing
                            & invaderBullets .~ []
                            & events .~ [])
