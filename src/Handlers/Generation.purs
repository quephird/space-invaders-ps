module Handlers.Generation where

import Prelude ( Unit()
               , (#), ($), (+), (-), (*), (==), (<), (>=), (&&)
               , bind, map, mod, otherwise, return, unit )

import Control.Monad.Eff ( Eff(), foreachE )
import Control.Monad.Eff.Random ( RANDOM(), random )
import Control.Monad.ST ( ST(), STRef()
                        , modifySTRef, readSTRef )
import Data.Array ( concat, cons )
import Data.Date ( Now()
                 , nowEpochMilliseconds )
import Data.Int ( fromNumber )
import Data.Maybe ( Maybe(..) )
import Data.Maybe.Unsafe ( fromJust )
import Data.Time ( Milliseconds(..), Seconds(..)
                 , toSeconds )
import Math ( (%), floor )
import Optic.Core ( (^.), (.~), (%~) )

import qualified Entities.Bullet as B
import qualified Entities.Event as V
import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as M
import qualified Entities.Star as T
import Helpers.Lens ( (&) )

createPlayerBullet :: forall g eff. STRef g G.Game
                   -> Eff ( st :: ST g | eff ) G.Game
createPlayerBullet gRef = do
  g <- readSTRef gRef
  let currBullet = g ^. G.playerBullet
      go Nothing = do
        let newPlayerBullet = Just $ B.makePlayerBullet (g ^. G.playerX) (g ^. G.playerY)
        modifySTRef gRef (\g -> g # G.playerBullet .~ newPlayerBullet)
      go _       = modifySTRef gRef (\g -> g)
  go currBullet

generateStars :: forall g eff. STRef g G.Game
              -> Eff ( random :: RANDOM
                     , st :: ST g | eff ) G.Game
generateStars gRef = do
  g <- readSTRef gRef
  coinToss <- random
  let currStars = g ^. G.stars
      maybeMakeNewStar coinToss gRef | coinToss < 0.25 = do
        x <- random
        let newStar = T.makeStar (g^.G.w * x) (g^.G.h)
        modifySTRef gRef (\g -> g # G.stars %~ (cons newStar))
      maybeMakeNewStar _ gRef | otherwise = do
        readSTRef gRef

  maybeMakeNewStar coinToss gRef

-- ACHTUNG!!! We don't check for strict equality here, only
-- that the time into the cycle is within the time between
-- frames passed in. If we checked for strict equality, we'd hardly
-- ever generate anything; if we checked for any longer length of time,
-- we'd risk generating entities twice or more per cycle.
isBeginningOfCycle :: Milliseconds
                   -> Milliseconds
                   -> Milliseconds
                   -> Prim.Boolean
isBeginningOfCycle (Milliseconds t) (Milliseconds c) (Milliseconds d) =
  t >= c && t % c < d

-- TODO: Look into why bullets are sometimes either not generated on cycle or
--       generated twice.
generateMysteryShipBullets :: forall g eff. STRef g G.Game
                           -> Eff ( now :: Now
                                  , random :: RANDOM
                                  , st :: ST g | eff ) G.Game
generateMysteryShipBullets gRef = do
  g <- readSTRef gRef
  currTime <- nowEpochMilliseconds
  let currMysteryShip = g ^. G.mysteryShip
      frameDuration   = g ^. G.frameDuration
      timeIntoGame    = currTime - g ^. G.startTime
      cycleDuration   = Milliseconds 500.0
      beginningOfCycle = isBeginningOfCycle timeIntoGame cycleDuration frameDuration

      go (Just m) true = do
        let newBullets = map (\dx -> B.makeMysteryBullet (m^.M.x+dx) (m^.M.y+25.0)) [ -25.0, 0.0, 25.0 ]
        modifySTRef gRef (\g -> g # G.invaderBullets %~ (\currBullets -> concat [ currBullets, newBullets ])
                                  & G.events %~ (cons $ V.Event V.NewMysteryBullet V.New))
      go _ _           = modifySTRef gRef (\g -> g)

  go currMysteryShip beginningOfCycle

generateInvaderBullets :: forall g eff. STRef g G.Game
                       -> Eff ( random :: RANDOM
                              , st :: ST g | eff ) G.Game
generateInvaderBullets gRef = do
  g <- readSTRef gRef
  let currInvaders = g ^. G.invaders
      maybeMakeNewInvaderBullet r i | r < 0.005 = do
        let newInvaderBullet = B.makeInvaderBullet (i^.I.x) (i^.I.y+25.0)
        modifySTRef gRef (\g -> g # G.invaderBullets %~ (cons newInvaderBullet)
                                  & G.events %~ (cons $ V.Event V.NewInvaderBullet V.New))
      maybeMakeNewInvaderBullet _ _ | otherwise = do
        modifySTRef gRef (\g -> g)

  foreachE currInvaders $ \i -> do
    r <- random
    maybeMakeNewInvaderBullet r i
    return unit
  readSTRef gRef

possiblyGenerateMysteryShip :: forall g eff. STRef g G.Game
                            -> Eff ( now :: Now
                                   , random :: RANDOM
                                   , st :: ST g | eff ) G.Game
possiblyGenerateMysteryShip gRef = do
  g <- readSTRef gRef
  currTime <- nowEpochMilliseconds
  let frameDuration   = g ^. G.frameDuration
      timeIntoGame    = currTime - g ^. G.startTime
      cycleDuration   = Milliseconds 7000.0
      currMysteryShip = g ^. G.mysteryShip
      beginningOfCycle = isBeginningOfCycle currTime cycleDuration frameDuration

      go Nothing true = do
        let newEvent = V.Event V.NewMysteryShip V.New
        modifySTRef gRef (\g -> g # G.mysteryShip .~ (Just $ M.makeMysteryShip (-100.0) 75.0)
                                  & G.events %~ (cons newEvent))
      go _ _ =
        modifySTRef gRef (\g -> g)

  go currMysteryShip beginningOfCycle

generateEverything :: forall g eff. STRef g G.Game
                   -> Eff ( now :: Now
                          , random :: RANDOM
                          , st :: ST g | eff ) G.Game
generateEverything gRef = do
  foreachE [ generateStars
           , generateInvaderBullets
           , generateMysteryShipBullets
           , possiblyGenerateMysteryShip
           ] (\f -> do
                      f gRef
                      return unit)
  readSTRef gRef
