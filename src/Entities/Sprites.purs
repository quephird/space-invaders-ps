module Entities.Sprites where

import Prelude ( ($)
               , bind, return )

import Control.Monad.Eff ( Eff() )
import Graphics.Canvas ( Canvas(), CanvasImageSource() )
import Optic.Core ( lens )

import Helpers.Image ( makeCanvasImageSource, getWidth )

data Sprites = Sprites
  { player :: CanvasImageSource
  , playerNew :: CanvasImageSource
  , playerBullet :: CanvasImageSource
  , invader :: Array CanvasImageSource
  , invaderBullet :: CanvasImageSource
  , shotInvader :: CanvasImageSource
  , deadInvader :: CanvasImageSource
  , mysteryShip :: Array CanvasImageSource
  , mysteryBullet :: CanvasImageSource
  , lives :: CanvasImageSource
  }

lives = lens (\(Sprites s) -> s.lives)
             (\(Sprites s) lives' -> Sprites (s { lives = lives' }))
player = lens (\(Sprites s) -> s.player)
              (\(Sprites s) player' -> Sprites (s { player = player' }))
playerNew = lens (\(Sprites s) -> s.playerNew)
                 (\(Sprites s) playerNew' -> Sprites (s { playerNew = playerNew' }))
playerBullet = lens (\(Sprites s) -> s.playerBullet)
                    (\(Sprites s) playerBullet' -> Sprites (s { playerBullet = playerBullet' }))
invader = lens (\(Sprites s) -> s.invader)
               (\(Sprites s) invader' -> Sprites (s { invader = invader' }))
invaderBullet = lens (\(Sprites s) -> s.invaderBullet)
                     (\(Sprites s) invaderBullet' -> Sprites (s { invaderBullet = invaderBullet' }))
shotInvader = lens (\(Sprites s) -> s.shotInvader)
                   (\(Sprites s) shotInvader' -> Sprites (s { shotInvader = shotInvader' }))
deadInvader = lens (\(Sprites s) -> s.deadInvader)
                   (\(Sprites s) deadInvader' -> Sprites (s { deadInvader = deadInvader' }))
mysteryShip = lens (\(Sprites s) -> s.mysteryShip)
                   (\(Sprites s) mysteryShip' -> Sprites (s { mysteryShip = mysteryShip' }))
mysteryBullet = lens (\(Sprites s) -> s.mysteryBullet)
                     (\(Sprites s) mysteryBullet' -> Sprites (s { mysteryBullet = mysteryBullet' }))

loadSprites :: forall eff. Eff ( canvas :: Canvas | eff ) Sprites
loadSprites = do
  playerSprite <- makeCanvasImageSource "images/player.png"
  playerNewSprite <- makeCanvasImageSource "images/playerNew.png"
  playerBulletSprite <- makeCanvasImageSource "images/playerBullet.png"
  invaderSprite1 <- makeCanvasImageSource "images/invader1.png"
  invaderSprite2 <- makeCanvasImageSource "images/invader2.png"
  invaderBulletSprite <- makeCanvasImageSource "images/invaderBullet.png"
  shotInvaderSprite <- makeCanvasImageSource "images/invaderShot.png"
  deadInvaderSprite <- makeCanvasImageSource "images/invaderDead.png"
  lifeSprite <- makeCanvasImageSource "images/life.png"
  mysteryShipSprite1 <- makeCanvasImageSource "images/mysteryShip1.png"
  mysteryShipSprite2 <- makeCanvasImageSource "images/mysteryShip2.png"
  mysteryShipSprite3 <- makeCanvasImageSource "images/mysteryShip3.png"
  mysteryShipSprite4 <- makeCanvasImageSource "images/mysteryShip4.png"
  mysteryBulletSprite <- makeCanvasImageSource "images/mysteryBullet.png"

  return $ Sprites
    { player: playerSprite
    , playerNew: playerNewSprite
    , playerBullet: playerBulletSprite
    , invader: [ invaderSprite1, invaderSprite2 ]
    , invaderBullet: invaderBulletSprite
    , shotInvader: shotInvaderSprite
    , deadInvader: deadInvaderSprite
    , lives: lifeSprite
    , mysteryShip: [ mysteryShipSprite1, mysteryShipSprite2
                   , mysteryShipSprite3, mysteryShipSprite4 ]
    , mysteryBullet: mysteryBulletSprite
    }
