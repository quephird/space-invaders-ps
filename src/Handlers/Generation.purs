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
import Math ( floor )
import Optic.Core ( (^.), (.~), (%~) )

import qualified Entities.Bullet as B
import qualified Entities.Event as V
import qualified Entities.Game as G
import qualified Entities.Invader as I
import qualified Entities.MysteryShip as M
import qualified Entities.Star as T
import Helpers.Lens ( (&) )

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

-- TODO: Move all generate/create functions into new module Generation
createPlayerBullet :: forall g eff. STRef g G.Game
                   -> Eff ( st :: ST g | eff ) G.Game
createPlayerBullet gRef = do
  g <- readSTRef gRef
  let newPlayerBullet = B.makePlayerBullet (g ^. G.playerX) (g ^. G.playerY)
  modifySTRef gRef (\g -> g # G.playerBullets %~ (cons newPlayerBullet))

generateMysteryShipBullets :: forall g eff. STRef g G.Game
                           -> Eff ( now :: Now
                                  , random :: RANDOM
                                  , st :: ST g | eff ) G.Game
generateMysteryShipBullets gRef = do
  g <- readSTRef gRef
  currTime <- nowEpochMilliseconds
  let currMysteryShip = g ^. G.mysteryShip
      millisecondsIntoGame = currTime - g ^. G.startTime

      isOnBulletCycle (Milliseconds m) =
        let m' = fromJust $ fromNumber $ floor m
        in
          m' >= 500 && m' `mod` 500 < 50

      go (Just m) true = do
        let newBullets = map (\dx -> B.makeInvaderBullet (m^.M.x+dx) (m^.M.y+25.0)) [ -25.0, 0.0, 25.0 ]
        modifySTRef gRef (\g -> g # G.invaderBullets %~ (\currBullets -> concat [ currBullets, newBullets ])
                                  & G.events %~ (cons $ V.Event V.NewInvaderBullet V.New))
      go _ _           = modifySTRef gRef (\g -> g)

  go currMysteryShip $ isOnBulletCycle millisecondsIntoGame

-- TODO: OMFG THIS IS ABSOLUTE 💩 MAKE THIS BETTER
generateInvaderBullets :: forall g eff. STRef g G.Game
                       -> Eff ( random :: RANDOM
                              , st :: ST g | eff ) G.Game
generateInvaderBullets gRef = do
  g <- readSTRef gRef
  let currInvaders = g ^. G.invaders

  foreachE currInvaders $ \i -> do
    r <- random
    maybeMakeNewInvaderBullet r i gRef
    return unit
  readSTRef gRef where
    maybeMakeNewInvaderBullet r i gRef | r < 0.005 = do
      let newInvaderBullet = B.makeInvaderBullet (i^.I.x) (i^.I.y+25.0)
      modifySTRef gRef (\g -> g # G.invaderBullets %~ (cons newInvaderBullet)
                                & G.events %~ (cons $ V.Event V.NewInvaderBullet V.New))
    maybeMakeNewInvaderBullet _ _ gRef | otherwise = do
      readSTRef gRef

isBeginningOfCycle :: Seconds
                   -> Int
                   -> Prim.Boolean
isBeginningOfCycle (Seconds t) c =
  let t' = fromJust $ fromNumber $ floor t
  in
    t' >= c && t' `mod` c == 0

possiblyGenerateMysteryShip :: forall g eff. STRef g G.Game
                            -> Eff ( now :: Now
                                   , random :: RANDOM
                                   , st :: ST g | eff ) G.Game
possiblyGenerateMysteryShip gRef = do
  g <- readSTRef gRef
  currTime <- nowEpochMilliseconds
  let secondsIntoGame = toSeconds $ currTime - g^.G.startTime
      currMysteryShip = g ^. G.mysteryShip
      beginningOfCycle = isBeginningOfCycle secondsIntoGame 7
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
