module Render where

import Prelude ( ($), (+), (-), (*)
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
import Math ( floor )
import Optic.Core ( (^.) )

import qualified Data.Game.Game as G
import qualified Data.Game.Invader as I

-- TODO: Move all rendering stuff to new Render module
chooseSprite sprites (Seconds s) idx =
  let n = length sprites
      idx' = toNumber idx
      spriteIdx = (fromJust $ fromNumber $ floor $ s * 2.0 + idx') `mod` 2
  in
    fromJust $ sprites !! spriteIdx

renderEnemies ctx g = do
  currentTime <- nowEpochMilliseconds
  let invaderSprites = g ^. G.invaderSprites
      invaders = g ^. G.invaders
      secondsIntoGame = toSeconds $ currentTime - (g ^. G.startTime)
  foreachE invaders $ \i -> do
    let sprite = chooseSprite invaderSprites secondsIntoGame (i ^. I.idx)
    drawImage ctx
              sprite
              (i ^. I.x)
              (i ^. I.y)
    return unit

renderPlayer ctx g = do
  drawImage ctx
            (g ^. G.playerSprite)
            (g ^. G.playerX)
            (g ^. G.playerY)
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

    renderEnemies ctx g
    renderPlayer ctx g
    render gRef