module Main where

import Prelude ( Unit()
               , (>>=), ($), (+), (-)
               , bind, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST()
                        , STRef(..)
                        , modifySTRef
                        , newSTRef
                        , readSTRef )
import DOM ( DOM() )
import DOM.Event.EventTarget ( EventListener(..)
                             , addEventListener
                             , eventListener )
import DOM.Event.Types ( Event()
                       , EventType(..) )
import DOM.HTML ( window )
import DOM.HTML.Types ( windowToEventTarget )
import DOM.Timer ( Timeout()
                 , Timer()
                 , interval
                 , timeout )
import Graphics.Canvas ( Canvas() )

import Control.Monad.Eff.Console ( CONSOLE() )
import Control.Monad.Eff.Console.Unsafe ( logAny )

type Player =
  { x :: Number
  , y :: Number
  }

type Game =
  { player :: Player
  }

setup :: forall eff. Eff eff Game
setup = do
  return { player: { x: 200.0, y: 200.0 }}

update :: forall eff g. STRef g Game
       -> Eff ( console :: CONSOLE
              , st :: ST g | eff ) Unit
update gRef = do
  modifySTRef gRef (\g -> { player: { x: g.player.x + 1.0
                                    , y: g.player.x - 1.0 }})
  return unit

render :: forall eff g. STRef g Game
       -> Eff ( canvas :: Canvas
              , console :: CONSOLE
              , st :: ST g
              , timer :: Timer | eff ) Timeout
render gRef = do
  timeout 50 $ do
    g <- readSTRef gRef
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
  gRef <- newSTRef { player: { x: 200.0, y: 200.0 }}

  addEventListener (EventType "keydown")
                   (eventListener (\e -> logAny "Key pressed!!!"))
                   false
                   (windowToEventTarget globalWindow)

  gameLoop gRef
