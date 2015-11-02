module Helpers.Image where

import Prelude ( ($), (-), (*) )

import Control.Monad.Eff (Eff(..))
import Data.Int ( toNumber )
import Graphics.Canvas ( Canvas(..), CanvasImageSource(..), Context2D()
                       , drawImage )

foreign import makeCanvasImageSource :: String -> forall eff. Eff (canvas :: Canvas | eff) CanvasImageSource

foreign import getWidth :: CanvasImageSource -> Int
foreign import getHeight :: CanvasImageSource -> Int

drawImageCentered :: forall eff. Context2D
                  -> CanvasImageSource
                  -> Number
                  -> Number
                  -> Eff (canvas :: Canvas | eff) Context2D
drawImageCentered ctx img x y = do
  let w = toNumber $ getWidth img
      h = toNumber $ getHeight img
  drawImage ctx img (x - 0.5*w) (y - 0.5*h)
