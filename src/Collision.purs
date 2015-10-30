module Collision where

import Prelude ( (#), ($), (*), (-), (&&), (<)
               , bind, map, return )

import Control.Monad.Eff ( Eff() )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( (\\), filter, length )
import Data.Tuple ( Tuple(..) )
import Math ( abs )
import Optic.Core ( (^.), (.~), (+~) )

import qualified Entities.Bullet as B
import qualified Entities.Game as G
import qualified Entities.Invader as I
import Util ( (&) )

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

class Shootable a where
  isShot :: a -> B.Bullet -> Boolean

instance shootableInvader :: Shootable I.Invader where
  isShot i b =
    abs(i^.I.x - b^.B.x) < 25.0 &&
    abs(i^.I.y - b^.B.y) < 25.0

checkInvadersShot :: forall eff g. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
checkInvadersShot gRef = do
  g <- readSTRef gRef

  -- Get current bullets and invaders
  -- Determine pairs of colliding bullets and invaders
  -- Remove dead bullets and invaders
  -- Add points for each shot invader

  -- TODO: Add sound events for each shot invader

  let currBullets = g ^. G.playerBullets
      currInvaders = g ^. G.invaders
      collisions = filter (\(Tuple i b) -> isShot i b) $ do
        i <- currInvaders
        b <- currBullets
        return $ Tuple i b

      -- TODO: Think about how scoring should be best handled.
      deadInvaders = map (\(Tuple i _) -> i) collisions
      deadBullets  = map (\(Tuple _ b) -> b) collisions
      newInvaders  = currInvaders \\ deadInvaders
      newBullets   = currBullets \\ deadBullets
      newPoints    = 100 * length deadInvaders

  modifySTRef gRef (\g -> g # G.invaders .~ newInvaders
                            & G.playerBullets .~ newBullets
                            & G.score +~ newPoints)
