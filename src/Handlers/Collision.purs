module Handlers.Collision where

import Prelude ( (#), ($), (*), (-), (&&), (<), (>), (==)
               , bind, map, otherwise, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( (\\), concat, cons, filter, length, nub )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Maybe ( Maybe(..) )
import Data.Tuple ( Tuple(..) )
import Math ( abs )
import Optic.Core ( (^.), (.~), (+~), (-~) )

import qualified Entities.Bullet as B
import qualified Entities.Game as G
import qualified Entities.Event as V
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as M
import qualified Entities.Player as P
import Helpers.Lens ( (&) )

-- import Control.Monad.Eff.Console ( CONSOLE() )
-- import Control.Monad.Eff.Console.Unsafe ( logAny )

class Shootable a where
  isShot :: a -> B.Bullet -> Boolean

instance shootableInvader :: Shootable I.Invader where
  isShot i b =
    i^.I.status == I.Alive &&
    abs(i^.I.x - b^.B.x) < 25.0 &&
    abs(i^.I.y - b^.B.y) < 25.0

instance shootableMysteryShip :: Shootable M.MysteryShip where
  isShot m b =
    m^.M.status == M.Alive &&
    abs(m^.M.x - b^.B.x) < 50.0 &&
    abs(m^.M.y - b^.B.y) < 25.0

-- TODO: Player can only be shot if in Alive status
instance shootablePlayer :: Shootable P.Player where
  isShot p b =
    p^.P.status == P.Alive &&
    abs(p^.P.x - b^.B.x) < 25.0 &&
    abs(p^.P.y - b^.B.y) < 25.0

checkPlayerShot :: forall eff g. STRef g G.Game
                -> Eff ( now :: Now
                       , st :: ST g | eff ) G.Game
checkPlayerShot gRef = do
  g <- readSTRef gRef
  let currBullets  = g ^. G.invaderBullets
      player       = g ^. G.player
      isDead       = (>0) $ length $ filter (isShot player) currBullets

      go true = do
        newStartTime <- nowEpochMilliseconds
        let newPlayer = P.resetPlayer (g^.G.player) (0.5*g^.G.w) newStartTime
        modifySTRef gRef (\g -> g # G.lives -~ 1
                                  & G.playerBullet .~ Nothing
                                  & G.invaderBullets .~ []
                                  & G.events .~ [V.Event V.PlayerShot V.New]
                                  & G.player .~ newPlayer)
      go _    = modifySTRef gRef (\g -> g)

  go isDead

checkInvadersShot :: forall eff g. STRef g G.Game
                  -> Eff ( st :: ST g | eff ) G.Game
checkInvadersShot gRef = do
  g <- readSTRef gRef
  let currBullet   = g ^. G.playerBullet
      go Nothing   = modifySTRef gRef (\g -> g)
      go (Just b)  = do
        let currInvaders = g ^. G.invaders
            currEvents   = g ^. G.events
            collisions   = filter (\(Tuple i b) -> isShot i b) $ do
                             i <- currInvaders
                             return $ Tuple i b

            shotInvaders  = nub $ map (\(Tuple i _) -> i # I.status .~ I.Shot) collisions
            otherInvaders = currInvaders \\ shotInvaders
            newInvaders   = concat $ [otherInvaders, shotInvaders]
            -- TODO: Think about how scoring should be best handled.
            newPoints     = 100 * length shotInvaders

            newBullet | length collisions > 0 = Nothing
                      | otherwise             = currBullet
            newEvents | length shotInvaders > 0 = cons (V.Event V.InvaderShot V.New) currEvents
                      | otherwise               = currEvents

        modifySTRef gRef (\g -> g # G.invaders .~ newInvaders
                                  & G.playerBullet .~ newBullet
                                  & G.score +~ newPoints
                                  & G.events .~ newEvents)

  go currBullet

checkMysteryShipShot :: forall eff g. STRef g G.Game
                     -> Eff ( st :: ST g | eff ) G.Game
checkMysteryShipShot gRef = do
  g <- readSTRef gRef
  let currBullet   = g ^. G.playerBullet
      mysteryShip  = g ^. G.mysteryShip

      go (Just b) (Just m) = do
        let currEvents   = g ^. G.events
            collisions   = filter (\(Tuple m b) -> isShot m b) $ do
                             return $ Tuple m b
            newMysteryShip = Just $ m # M.status .~ M.Shot

            go' true = modifySTRef gRef (\g -> g # G.playerBullet .~ Nothing
                                                 & G.mysteryShip .~ newMysteryShip
                                                 & G.score +~ 1000
                                                 & G.events .~ [V.Event V.MysteryShipShot V.New])
            go' _    = modifySTRef gRef (\g -> g)
        go' $ isShot m b
      go _ _ = modifySTRef gRef (\g -> g)
  go currBullet mysteryShip

checkForAllCollisions gRef = do
  foreachE [ checkPlayerShot
           , checkInvadersShot
           , checkMysteryShipShot
           ] (\f -> do
                      f gRef
                      return unit)
  readSTRef gRef



