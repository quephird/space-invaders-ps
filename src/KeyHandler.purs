module KeyHandler where

import Prelude ( ($), (#) )

import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef )
import DOM.Event.EventTarget ( EventListener()
                             , eventListener )
import DOM.Event.Types ( Event() )
import Optic.Core ( (+~) )
import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.Game.Game as G

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
          -> EventListener ( st :: ST g  | eff )
onKeydown gRef = eventListener $ \evt -> do
  let keyboardEvent = eventToKeyboardEvent evt
      code = keyboardEvent.keyCode
      key = case code of
              32 -> SpaceBar
              37 -> Left
              39 -> Right
              _  -> Other
  movePlayer key gRef
