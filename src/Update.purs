module Update where

import Prelude ( Unit()
               , bind, return, unit )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef() )

import qualified Entities.Game as G
import qualified Motion as M

update :: forall eff g. STRef g G.Game
       -> Eff ( st :: ST g | eff ) Unit
update gRef = do
  M.movePlayerBullets gRef
  return unit
