module Main where

import Prelude ( Unit()
               , ($)
               , bind, mod, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.Eff.Random ( RANDOM() )
import Control.Monad.ST ( ST(), STRef()
                        , newSTRef, readSTRef )
import Data.Date ( Now() )
import Data.Int ( toNumber )
import DOM ( DOM() )
import DOM.Event.EventTarget ( addEventListener )
import DOM.Event.Types ( EventType(..) )
import DOM.HTML ( window )
import DOM.HTML.Types ( windowToEventTarget )
import DOM.Node.NonElementParentNode ( getElementById )
import DOM.Node.Types ( ElementId(..)
                      , elementToEventTarget )
import Data.Time ( Milliseconds(..) )
import DOM.Timer ( Timeout(), Timer()
                 , interval, timeout )
import Graphics.Canvas ( Canvas() )
import Optic.Core ( (^.) )

import qualified Entities.Game as G
import qualified Handlers.Event as V
import qualified Handlers.Keyboard as K
import qualified Handlers.Rendering as R
import qualified Update as U

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

frameDuration :: Int
frameDuration = 50
w :: Number
w = 800.0
h :: Number
h = 750.0

gameLoop :: forall eff g. STRef g G.Game
         -> Eff ( canvas :: Canvas
                -- , console :: CONSOLE
                , now :: Now
                , st :: ST g
                , timer :: Timer | eff ) Timeout
gameLoop gRef = do
  timeout frameDuration $ do
    R.render gRef
    V.processEvents gRef
    V.clearHandledEvents gRef
    gameLoop gRef

main :: forall eff g. Eff ( canvas :: Canvas
                          -- , console :: CONSOLE
                          , dom :: DOM
                          , now :: Now
                          , random :: RANDOM
                          , st :: ST g
                          , timer :: Timer | eff ) Timeout
main = do
  g <- G.makeGame w h $ Milliseconds $ toNumber frameDuration
  gRef <- newSTRef g

  globalWindow <- window
  addEventListener (EventType "keydown")
                   (K.onKeydown gRef)
                   false
                   (windowToEventTarget globalWindow)

  interval frameDuration $ U.update gRef
  gameLoop gRef
