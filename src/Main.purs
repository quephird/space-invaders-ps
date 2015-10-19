module Main where

import Prelude ( Functor, Unit()
               , (>>=), (<$>), ($), (+), (-), (*), (++), (#)
               , bind, mod, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST()
                        , STRef()
                        , modifySTRef
                        , newSTRef
                        , readSTRef )
import Data.Date ( Now() )
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
import Graphics.Canvas ( Canvas() )
import Optic.Core ( (^.), (..), (+~) )
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Eff.Console ( CONSOLE() )
import Control.Monad.Eff.Console.Unsafe ( logAny )

import qualified Data.Game.Enemies as E
import qualified Data.Game.Game as G
import qualified Data.Game.Invader as I
import qualified Data.Game.Player as P
import qualified Data.Game.Sprites as S
import qualified Render as R

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

update :: forall eff g. STRef g G.Game
       -> Eff ( console :: CONSOLE
              , st :: ST g | eff ) Unit
update gRef = do
  return unit

gameLoop :: forall eff g. STRef g G.Game
         -> Eff ( canvas :: Canvas
                , console :: CONSOLE
                , now :: Now
                , st :: ST g
                , timer :: Timer | eff ) Timeout
gameLoop gRef = do
  interval 200 $ update gRef
  R.render gRef

main :: forall eff g. Eff ( canvas :: Canvas
                          , console :: CONSOLE
                          , dom :: DOM
                          , now :: Now
                          , st :: ST g
                          , timer :: Timer | eff ) Timeout
main = do
  globalWindow <- window
  g <- G.makeGame 800.0 600.0
  gRef <- newSTRef g

  addEventListener (EventType "keydown")
                   (onKeydown gRef)
                   false
                   (windowToEventTarget globalWindow)

  gameLoop gRef
