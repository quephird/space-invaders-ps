module Helpers.Audio where

import Prelude ( Unit() )

import Control.Monad.Eff (Eff(..))

foreign import data Sound :: *

-- TODO: Need to specify Sound in the resultant Eff.
--       Need to move this into a different namespace.
foreign import loadSound :: String
                         -> forall eff. Eff ( eff ) Sound

foreign import playSound :: Sound
                         -> forall eff. Eff ( eff ) Unit

foreign import loopSound :: Sound
                         -> forall eff. Eff ( eff ) Unit

foreign import stopSound :: Sound
                         -> forall eff. Eff ( eff ) Unit
