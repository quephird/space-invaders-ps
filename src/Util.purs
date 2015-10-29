module Util where

import Prelude ( ($), flip )

-- This combinator is used below to allow for chaining
-- of lens operations. The implementation was taken directly from here:
--
-- https://hackage.haskell.org/package/lens-4.13/docs/Control-Lens-Lens.html#g:4
(&) :: forall a b. a -> (a -> b) -> b
(&) = flip ($)
