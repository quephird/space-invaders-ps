module KeyHandler where

import Prelude ( ($), (#)
               , return )

import Control.Monad.Eff ( Eff(), Pure() )
import Control.Monad.Eff.Class ( liftEff )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import DOM.Event.EventTarget ( EventListener()
                             , eventListener )
import DOM.Event.Types ( Event() )
import Optic.Core ( (+~) )
import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.Game.Game as G

data Key = Left | Right | SpaceBar | Other

movePlayer :: forall g eff. Key
           -> STRef g G.Game
           -> Eff ( st :: ST g | eff ) G.Game
movePlayer key gRef = do
  let dx = case key of
             Left -> -10.0
             Right -> 10.0
             _ -> 0.0
  modifySTRef gRef (\g -> g # G.playerX +~ dx)

-- TODO: Figure out how to do inline this into onKeydown, and
--       if there's a better way to return gRef unchanged
respondToKey :: forall g eff. Key
             -> STRef g G.Game
             -> Eff ( st :: ST g | eff ) G.Game
respondToKey Left gRef = movePlayer Left gRef
respondToKey Right gRef = movePlayer Right gRef
respondToKey SpaceBar gRef = G.createPlayerBullet gRef
respondToKey _ _ = readSTRef gRef

type KeyboardEventMini =
  { keyCode :: Int
  }
eventToKeyboardEvent :: Event -> KeyboardEventMini
eventToKeyboardEvent = unsafeCoerce

onKeydown :: forall g eff. STRef g G.Game
          -> EventListener ( st :: ST g | eff )
onKeydown gRef = eventListener $ \evt -> do
  let keyboardEvent = eventToKeyboardEvent evt
      code = keyboardEvent.keyCode
      key = case code of
              32 -> SpaceBar
              37 -> Left
              39 -> Right
              _  -> Other
  respondToKey key gRef
