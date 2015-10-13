module Main where

import Prelude ( Functor, Unit()
               , (>>=), ($), (+), (-), (*), (++), (#)
               , bind, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.ST ( ST()
                        , STRef()
                        , modifySTRef
                        , newSTRef
                        , readSTRef )
import Data.Maybe ( Maybe(..) )
import DOM ( DOM() )
import DOM.Event.EventTarget ( EventListener()
                             , addEventListener
                             , eventListener )
import DOM.Event.Types ( Event()
                       , EventType(..)
                       , KeyboardEvent() )
import DOM.HTML ( window )
import DOM.HTML.Types ( windowToEventTarget )
import DOM.Timer ( Timeout()
                 , Timer()
                 , interval
                 , timeout )
import Graphics.Canvas ( Canvas()
                       , Rectangle()
                       , fillRect
                       , getCanvasElementById
                       , getContext2D
                       , rect
                       , setFillStyle
                       )
import Optic.Core ( (*~), (^.), (..), (+~)
                  , Lens(), LensP(), Getter()
                  , lens, view )
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Eff.Console ( CONSOLE() )
import Control.Monad.Eff.Console.Unsafe ( logAny )

data Player = Player
  { x :: Number
  , y :: Number
  }

data Game = Game
  { player :: Player
  , w :: Number
  , h :: Number
  }

player = lens (\(Game g) -> g.player)
              (\(Game g) player' -> Game (g { player = player' }))
x = lens (\(Player p) -> p.x)
         (\(Player p) x' -> Player (p { x = x' }))
y = lens (\(Player p) -> p.y)
         (\(Player p) y' -> Player (p { y = y' }))

playerX :: Lens Game Game Number Number
playerX = player .. x
playerY :: Lens Game Game Number Number
playerY = player .. y


data Key = Left | Right | SpaceBar | Other

movePlayer key gRef = do
  let dx = case key of
             Left -> -10.0
             Right -> 10.0
             _ -> 0.0
  modifySTRef gRef (\g -> g # playerX +~ dx)

type KeyboardEventMini =
  { keyCode :: Int
  }
eventToKeyboardEvent :: Event -> KeyboardEventMini
eventToKeyboardEvent = unsafeCoerce

onKeydown :: forall g eff. STRef g Game
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

setup :: Game
setup =
  let w = 800.0
      h = 600.0 in
  Game { player: Player { x: 0.5*w, y: 0.9*h }
       , w: w
       , h: h }

update :: forall eff g. STRef g Game
       -> Eff ( console :: CONSOLE
              , st :: ST g | eff ) Unit
update gRef = do
  return unit

render :: forall eff g. STRef g Game
       -> Eff ( canvas :: Canvas
              , console :: CONSOLE
              , st :: ST g
              , timer :: Timer | eff ) Timeout
render gRef = do
  Just canvas <- getCanvasElementById "game"
  ctx <- getContext2D canvas
  timeout 50 $ do
    g <- readSTRef gRef

    setFillStyle "#FFFFFF" ctx
    fillRect ctx { x: 0.0
                 , y: 0.0
                 , w: 800.0
                 , h: 600.0}

    setFillStyle "#FF00FF" ctx
    fillRect ctx { x: g ^. playerX
                 , y: g ^. playerY
                 , w: 50.0
                 , h: 50.0}
    render gRef

gameLoop :: forall eff g. STRef g Game
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
  gRef <- newSTRef $ setup

  addEventListener (EventType "keydown")
                   (onKeydown gRef)
                   false
                   (windowToEventTarget globalWindow)

  gameLoop gRef
