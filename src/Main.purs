module Main where

import Prelude ( Unit()
               , ($)
               , bind, mod, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , newSTRef, readSTRef )
import Data.Date ( Now() )
import DOM ( DOM() )
import DOM.Event.EventTarget ( addEventListener )
import DOM.Event.Types ( EventType(..) )
import DOM.HTML ( window )
import DOM.HTML.Types ( windowToEventTarget )
import DOM.Node.NonElementParentNode ( getElementById )
import DOM.Node.Types ( ElementId(..)
                      , elementToEventTarget )
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

gameLoop :: forall eff g. STRef g G.Game
         -> Eff ( canvas :: Canvas
                -- , console :: CONSOLE
                , now :: Now
                , st :: ST g
                , timer :: Timer | eff ) Timeout
gameLoop gRef = do
  timeout 50 $ do
    R.render gRef
    V.processEvents gRef
    V.clearHandledEvents gRef
    gameLoop gRef

main :: forall eff g. Eff ( canvas :: Canvas
                          -- , console :: CONSOLE
                          , dom :: DOM
                          , now :: Now
                          , st :: ST g
                          , timer :: Timer | eff ) Timeout
main = do
  g <- G.makeGame 800.0 750.0
  gRef <- newSTRef g

  globalWindow <- window
  addEventListener (EventType "keydown")
                   (K.onKeydown gRef)
                   false
                   (windowToEventTarget globalWindow)

  interval 50 $ U.update gRef
  gameLoop gRef
