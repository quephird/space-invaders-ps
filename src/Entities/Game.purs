module Entities.Game where

import Prelude ( bind, flip, otherwise, return, unit
               , (#), ($), (+), (*), (<) )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.Eff.Random ( RANDOM(), random )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Control.MonadPlus ( guard )
import Data.Array ( cons )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Foldable ( foldl )
import Data.Time ( Milliseconds() )
import Graphics.Canvas ( Canvas()
                       , CanvasImageSource() )
import Optic.Core ( Lens()
                  , (..), (^.), (.~), (%~)
                  , lens )

import qualified Entities.Bullet as B
import qualified Entities.Enemies as E
import qualified Entities.Event as V
import qualified Entities.Invader as I
import qualified Entities.Player as P
import qualified Entities.Sounds as O
import qualified Entities.Sprites as S
import qualified Helpers.Audio as A
import Helpers.Lens ( (&) )

data Status = GameOver
            | Playing
            | Waiting

-- TODO: Need to introduce current level property.
data Game = Game
  { w :: Number
  , h :: Number
  , startTime :: Milliseconds
  , status    :: Status
  , score     :: Int
  , lives     :: Int
  , player    :: P.Player
  , playerBullets :: Array B.Bullet
  , invaderBullets :: Array B.Bullet
  , enemies   :: E.Enemies
  , events    :: Array V.Event
  , sprites   :: S.Sprites
  , sounds    :: O.Sounds
  }

w = lens (\(Game g) -> g.w)
         (\(Game g) w' -> Game (g { w = w' }))
h = lens (\(Game g) -> g.h)
         (\(Game g) h' -> Game (g { h = h' }))
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
playerBullets = lens (\(Game g) -> g.playerBullets)
                     (\(Game g) playerBullets' -> Game (g { playerBullets = playerBullets' }))
invaderBullets = lens (\(Game g) -> g.invaderBullets)
                      (\(Game g) invaderBullets' -> Game (g { invaderBullets = invaderBullets' }))
enemies = lens (\(Game g) -> g.enemies)
               (\(Game g) enemies' -> Game (g { enemies = enemies' }))
events = lens (\(Game g) -> g.events)
              (\(Game g) events' -> Game (g { events = events' }))

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

newPlayerBulletSound :: Lens Game Game A.Sound A.Sound
newPlayerBulletSound = sounds .. O.newPlayerBullet
newInvaderBulletSound :: Lens Game Game A.Sound A.Sound
newInvaderBulletSound = sounds .. O.newInvaderBullet
invaderShotSound :: Lens Game Game A.Sound A.Sound
invaderShotSound = sounds .. O.invaderShot

makeGame :: forall eff. Number
         -> Number
         -> Eff ( canvas :: Canvas
                , now :: Now | eff) Game
makeGame w h = do
  let player = P.makePlayer (0.5*w) (0.9*h)
  sprites <- S.loadSprites
  sounds <- O.loadAllSounds
  startTime <- nowEpochMilliseconds
  return $ Game
    { w:         w
    , h:         h
    , startTime: startTime
    , status:    Waiting
    , score:     0
    , lives:     3
    , player:    player
    , playerBullets: []
    , invaderBullets: []
    , enemies:   E.makeRegularLevel
    , events:    []
    , sprites:   sprites
    , sounds:    sounds
    }

startGame :: forall g eff. STRef g Game
          -> Eff ( st :: ST g | eff ) Game
startGame gRef = do
  g <- readSTRef gRef
  modifySTRef gRef (\g -> g # status .~ Playing)

createPlayerBullet :: forall g eff. STRef g Game
                   -> Eff ( st :: ST g | eff ) Game
createPlayerBullet gRef = do
  g <- readSTRef gRef
  let newPlayerBullet = B.makePlayerBullet (g ^. playerX) (g ^. playerY)
  modifySTRef gRef (\g -> g # playerBullets %~ (cons newPlayerBullet))

-- TODO: OMFG THIS IS ABSOLUTE 💩 MAKE THIS BETTER
generateInvaderBullets :: forall g eff. STRef g Game
                       -> Eff ( random :: RANDOM
                              , st :: ST g | eff ) Game
generateInvaderBullets gRef = do
  g <- readSTRef gRef
  let currInvaders = g ^. invaders

  -- For each of the invaders
  --   generate a random number
  --   if it's less than $MAGIC_NUMBER
  --     make another bullet at the position of the invaderShotSound
  --     add it to the current list of invader bullets
  --   else
  --     do nothing

  foreachE currInvaders $ \i -> do
    r <- random
    maybeMakeNewInvaderBullet r i gRef
    return unit
  readSTRef gRef where
    maybeMakeNewInvaderBullet r i gRef | r < 0.005 = do
      let newInvaderBullet = B.makeInvaderBullet (i^.I.x) (i^.I.y+25.0)
      modifySTRef gRef (\g -> g # invaderBullets %~ (cons newInvaderBullet)
                                & events %~ (cons $ V.Event V.NewInvaderBullet V.New))
    maybeMakeNewInvaderBullet _ _ gRef | otherwise = do
      readSTRef gRef
