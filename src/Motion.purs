module Motion where

import Prelude ( Unit()
               , (#), (<$>)
               , bind, map, return )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Optic.Core ( (^.), (.~), (-~) )

import qualified Entities.Bullet as B
import qualified Entities.Game as G

import Control.Monad.Eff.Console ( CONSOLE() )
import Control.Monad.Eff.Console.Unsafe ( logAny )

movePlayerBullets :: forall g eff. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
movePlayerBullets gRef = do
  g <- readSTRef gRef
  let playerBullets = g ^. G.playerBullets
      newPlayerBullets = map (\b -> b # B.y -~ 20.0) playerBullets
  modifySTRef gRef (\g -> g # G.playerBullets .~ newPlayerBullets)
