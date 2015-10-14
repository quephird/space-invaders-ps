module Main where

import Prelude ( Functor, Unit()
               , (>>=), (<$>), ($), (+), (-), (*), (++), (#)
               , bind, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST()
                        , STRef()
                        , modifySTRef
                        , newSTRef
                        , readSTRef )
import Data.Maybe ( Maybe(..) )
import Data.Maybe.Unsafe ( fromJust )
import Data.Nullable ( toMaybe )
import DOM ( DOM() )
import DOM.Event.EventTarget ( EventListener()
                             , addEventListener
                             , eventListener )
import DOM.Event.Types ( Event()
                       , EventType(..)
                       , KeyboardEvent() )
import DOM.HTML ( window )
import DOM.HTML.Types ( htmlDocumentToNonElementParentNode
                      , windowToEventTarget )
import DOM.HTML.Window ( document )
import DOM.Node.Element ( getAttribute )
import DOM.Node.NonElementParentNode ( getElementById )
import DOM.Node.Types ( Element()
                      , ElementId(..) )
import DOM.Timer ( Timeout()
                 , Timer()
                 , interval
                 , timeout )
import Graphics.Canvas ( Canvas()
                       , CanvasImageSource()
                       , Rectangle()
                       , drawImage
                       , fillRect
                       , getCanvasElementById
                       , getContext2D
                       , rect
                       , setFillStyle
                       )
import Optic.Core ( (*~), (^.), (..), (+~)
                  , lens )
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Eff.Console ( CONSOLE() )
import Control.Monad.Eff.Console.Unsafe ( logAny )

import qualified Data.Game.Game as G
import qualified Data.Game.Player as P
import qualified Data.Game.Sprites as S
import Graphics.Canvas.Image ( makeCanvasImageSource )

data Key = Left | Right | SpaceBar | Other

movePlayer key gRef = do
  let dx = case key of
             Left -> -10.0
             Right -> 10.0
             _ -> 0.0
  modifySTRef gRef (\g -> g # G.playerX +~ dx)

type KeyboardEventMini =
  { keyCode :: Int
  }
eventToKeyboardEvent :: Event -> KeyboardEventMini
eventToKeyboardEvent = unsafeCoerce

onKeydown :: forall g eff. STRef g G.Game
          -> EventListener ( console :: CONSOLE
                           , st :: ST g  | eff )
onKeydown gRef = eventListener $ \evt -> do
  let keyboardEvent = eventToKeyboardEvent evt
      code = keyboardEvent.keyCode
      key = case code of
              32 -> SpaceBar
              37 -> Left
              39 -> Right
              _  -> Other
  movePlayer key gRef

setup :: forall eff. Eff (console :: CONSOLE, canvas :: Canvas | eff) G.Game
setup = do
  playerSprite <- makeCanvasImageSource "images/player.png"
  invaderSprite1 <- makeCanvasImageSource "images/invader1.png"
  invaderSprite2 <- makeCanvasImageSource "images/invader2.png"
  let w = 800.0
      h = 600.0
      player = P.Player
        { x: 0.5*w
        , y: 0.9*h
        }
      sprites = S.Sprites
        { player: playerSprite
        , invader: [ invaderSprite1, invaderSprite2 ]
        }
  return $ G.Game
    { player: player
    , w: w
    , h: h
    , sprites: sprites
    }

update :: forall eff g. STRef g G.Game
       -> Eff ( console :: CONSOLE
              , st :: ST g | eff ) Unit
update gRef = do
  return unit

renderPlayer ctx g = do
  drawImage ctx
            (g ^. G.playerSprite)
            (g ^. G.playerX)
            (g ^. G.playerY)

render :: forall eff g. STRef g G.Game
       -> Eff ( canvas :: Canvas
              , console :: CONSOLE
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

    renderPlayer ctx g
    render gRef

gameLoop :: forall eff g. STRef g G.Game
         -> Eff ( canvas :: Canvas
                , console :: CONSOLE
                , st :: ST g
                , timer :: Timer | eff ) Timeout
gameLoop gRef = do
  interval 200 $ update gRef
  render gRef

main :: forall eff g. Eff ( canvas :: Canvas
                          , console :: CONSOLE
                          , dom :: DOM
                          , st :: ST g
                          , timer :: Timer | eff ) Timeout
main = do
  globalWindow <- window
  g <- setup
  gRef <- newSTRef g

  addEventListener (EventType "keydown")
                   (onKeydown gRef)
                   false
                   (windowToEventTarget globalWindow)

  gameLoop gRef
