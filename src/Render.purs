module Render where

import Prelude ( Unit()
               , ($), (+), (-), (*)
               , bind, mod, return, unit )

import Control.Monad.Eff ( Eff()
                         , foreachE )
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
                       , drawImage, fillRect, getCanvasElementById
                       , getContext2D, rect, setFillStyle
                       )
import Graphics.Canvas.Image ( drawImageCentered, getWidth )
import Math ( floor )
import Optic.Core ( (^.) )

import qualified Entities.Bullet as B
import qualified Entities.Game as G
import qualified Entities.Invader as I

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
              , st :: ST g
              , timer :: Timer | eff ) Timeout
render gRef = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  timeout 50 $ do
    g <- readSTRef gRef

    setFillStyle "#000000" ctx
    fillRect ctx { x: 0.0
                 , y: 0.0
                 , w: 800.0
                 , h: 600.0}

    -- renderScore ctx g
    foreachE [ renderEnemies
             , renderPlayer
             , renderPlayerBullets
             ] (\f -> f ctx g)
    -- renderEnemies ctx g
    -- renderPlayer ctx g
    -- renderPlayerBullets ctx g
    render gRef
