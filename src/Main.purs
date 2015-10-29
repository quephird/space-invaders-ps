module Main where

import Prelude ( Unit()
               , ($)
               , bind, mod, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , newSTRef )
import Data.Date ( Now() )
import DOM ( DOM() )
import DOM.Event.EventTarget ( addEventListener )
import DOM.Event.Types ( EventType(..) )
import DOM.HTML ( window )
import DOM.HTML.Types ( windowToEventTarget )
import DOM.Timer ( Timeout(), Timer()
                 , interval )
import Graphics.Canvas ( Canvas() )

import qualified Entities.Game as G
import qualified KeyHandler as K
import qualified Render as R
import qualified Update as U

import Control.Monad.Eff.Console ( CONSOLE() )

gameLoop :: forall eff g. STRef g G.Game
         -> Eff ( canvas :: Canvas
                -- , console :: CONSOLE
                , now :: Now
                , st :: ST g
                , timer :: Timer | eff ) Timeout
gameLoop gRef = do
  interval 50 $ U.update gRef
  R.render gRef

main :: forall eff g. Eff ( canvas :: Canvas
                          -- , console :: CONSOLE
                          , dom :: DOM
                          , now :: Now
                          , st :: ST g
                          , timer :: Timer | eff ) Timeout
main = do
  globalWindow <- window
  g <- G.makeGame 800.0 600.0
  gRef <- newSTRef g

  addEventListener (EventType "keydown")
                   (K.onKeydown gRef)
                   false
                   (windowToEventTarget globalWindow)

  gameLoop gRef
