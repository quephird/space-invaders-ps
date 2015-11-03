module Handlers.Rendering where

import Prelude ( Unit()
               , ($), (+), (-), (*)
               , bind, mod, return, show, unit )

import Control.Monad.Eff ( Eff()
                         , forE, foreachE )
import Control.Monad.ST ( ST(), STRef()
                        , readSTRef )
import Data.Array ( (!!), index, length )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Int ( fromNumber, toNumber )
import Data.Maybe ( Maybe(..) )
import Data.Maybe.Unsafe ( fromJust )
import Data.Time ( Seconds(..), toSeconds )
import DOM.Timer ( Timeout(), Timer()
                 , timeout )
import Graphics.Canvas ( Canvas(), CanvasImageSource(), Context2D(), Rectangle()
                       , drawImage, fillRect, fillText, getCanvasElementById
                       , getContext2D, rect, setFillStyle, setFont
                       )
import Math ( floor )
import Optic.Core ( (^.) )

import qualified Entities.Bullet as B
import qualified Entities.Game as G
import qualified Entities.Invader as I
import Helpers.Image ( drawImageCentered, getWidth )

renderScore :: forall eff g. Context2D
            -> G.Game
            -> Eff ( canvas :: Canvas
                   , st :: ST g | eff ) Unit
renderScore ctx g = do
  setFillStyle "#7700FF" ctx
  setFont "32pt Courier" ctx
  fillText ctx
           (show $ g ^. G.score)
           50.0
           40.0
  return unit

renderLives :: forall eff g. Context2D
            -> G.Game
            -> Eff ( canvas :: Canvas
                   , st :: ST g | eff ) Unit
renderLives ctx g = do
  let lives = g ^. G.lives
  forE 0.0 (toNumber lives) $ \i -> do
    drawImageCentered ctx
                      (g ^. G.lifeSprite)
                      (g ^. G.w - i * 32.0 - 50.0)
                      30.0
    return unit
  return unit

-- TODO: Need to have separate implementations for Patrol and Boss types.
--       Need to alternate invader sprites faster as the number of them
--         remaining gets smaller.
renderEnemies :: forall eff g. Context2D
              -> G.Game
              -> Eff ( canvas :: Canvas
                     , now :: Now
                     , st :: ST g | eff ) Unit
renderEnemies ctx g = do
  currentTime <- nowEpochMilliseconds
  let invaderSprites = g ^. G.invaderSprites
      invaders = g ^. G.invaders
      secondsIntoGame = toSeconds $ currentTime - (g ^. G.startTime)
  foreachE invaders $ \i -> do
    let sprite = chooseSprite invaderSprites secondsIntoGame (i ^. I.idx)
    drawImageCentered ctx
                      sprite
                      (i ^. I.x)
                      (i ^. I.y)
    return unit where
      chooseSprite sprites (Seconds s) idx =
        let idx' = toNumber idx
            spriteIdx = (fromJust $ fromNumber $ floor $ idx' + s * 2.0) `mod` 2
        in
          fromJust $ sprites !! spriteIdx

renderPlayer :: forall eff g. Context2D
             -> G.Game
             -> Eff ( canvas :: Canvas
                    , st :: ST g | eff ) Unit
renderPlayer ctx g = do
  drawImageCentered ctx
                    (g ^. G.playerSprite)
                    (g ^. G.playerX)
                    (g ^. G.playerY)
  return unit

renderPlayerBullets :: forall eff g. Context2D
                    -> G.Game
                    -> Eff ( canvas :: Canvas
                           , st :: ST g | eff ) Unit
renderPlayerBullets ctx g = do
  foreachE (g ^. G.playerBullets) $ \b -> do
    drawImageCentered ctx
                      (g ^. G.playerBulletSprite)
                      (b ^. B.x)
                      (b ^. B.y)
    return unit
  return unit

render :: forall eff g. STRef g G.Game
       -> Eff ( canvas :: Canvas
              , now :: Now
              , st :: ST g | eff ) Unit
render gRef = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  g <- readSTRef gRef

  setFillStyle "#000000" ctx
  fillRect ctx { x: 0.0
               , y: 0.0
               , w: g ^. G.w
               , h: g ^. G.h}

  foreachE [ renderScore
           , renderLives
           , renderEnemies
           , renderPlayer
           , renderPlayerBullets
           ] (\f -> f ctx g)
