module Entities.Star where

import Prelude ( ($), (*)
               , bind, return )

import Control.Monad.Eff.Random ( RANDOM(), random )
import Data.Array ( (..), replicateM )
import Optic.Core ( lens )

data Star = Star
  { x :: Number
  , y :: Number
  }

x = lens (\(Star p) -> p.x)
         (\(Star p) x' -> Star (p { x = x' }))
y = lens (\(Star p) -> p.y)
         (\(Star p) y' -> Star (p { y = y' }))

makeStar x y = Star
  { x: x
  , y: y
  }

-- TODO: Find out why I can't make makeRandomStar anonymous;
--       I shouldn't have to name this little function.
generateStars w h =
  replicateM 100 $ makeRandomStar
  where
    makeRandomStar = do
      x <- random
      y <- random
      return $ makeStar (w*x) (h*y)
      -- return x
    -- randomY = do
    --   r <- random
    --   h*r
