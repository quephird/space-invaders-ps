module Handlers.Keyboard where

import Prelude ( ($), (#)
               , bind, return )

import Control.Monad.Eff ( Eff(), Pure() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Date ( Now() )
import DOM.Event.EventTarget ( EventListener()
                             , eventListener )
import DOM.Event.Types ( Event() )
import Optic.Core ( (^.), (+~) )
import Unsafe.Coerce ( unsafeCoerce )

import qualified Entities.Game as G
import qualified Handlers.Generation as N
import qualified Helpers.Audio as A

data Key = Left | Right | SpaceBar | S | Other

-- TODO: Need to constrain player within margins
movePlayer :: forall g eff. Key
           -> STRef g G.Game
           -> Eff ( st :: ST g | eff ) G.Game
movePlayer key gRef = do
  let dx = case key of
             Left -> -10.0
             Right -> 10.0
             _ -> 0.0
  modifySTRef gRef (\g -> g # G.playerX +~ dx)

respondToKey :: forall g eff. Key
             -> G.Status
             -> STRef g G.Game
             -> Eff ( now :: Now
                    , st :: ST g | eff ) G.Game
respondToKey Left G.Playing gRef = movePlayer Left gRef
respondToKey Right G.Playing gRef = movePlayer Right gRef
respondToKey SpaceBar G.Playing gRef = do
  g <- readSTRef gRef
  A.playSound $ g ^. G.newPlayerBulletSound
  N.createPlayerBullet gRef

respondToKey S G.Waiting gRef = do
  G.startGame gRef

respondToKey S G.GameOver gRef = do
  G.restartGame gRef

respondToKey _ _ _ = readSTRef gRef

type KeyboardEventMini =
  { keyCode :: Int
  }
eventToKeyboardEvent :: Event -> KeyboardEventMini
eventToKeyboardEvent = unsafeCoerce

onKeydown :: forall g eff. STRef g G.Game
          -> EventListener ( now :: Now
                           , st :: ST g | eff )
onKeydown gRef = eventListener $ \evt -> do
  g <- readSTRef gRef
  let gameStatus = g ^. G.status
      keyboardEvent = eventToKeyboardEvent evt
      code = keyboardEvent.keyCode
      key = case code of
              32 -> SpaceBar
              37 -> Left
              39 -> Right
              83 -> S
              _  -> Other
  respondToKey key gameStatus gRef
