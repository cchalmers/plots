{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
import Plots
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (normal)

import Control.Monad (replicateM)

import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad.ST

-- | Take 500 samples from a random number generating function.
samples :: (forall s. Gen s -> ST s Double) -> [Double]
samples f = runST $ create >>= replicateM 500 . f

axis :: Axis Rasterific V2 Double
axis = r2Axis &~ do
  histogramPlot (samples $ normal 2 0.2)  $ do
    key "normal"
    normaliseSample .= pdf
  histogramPlot (samples $ beta 2 5)      $ do
    key "beta"
    normaliseSample .= pdf
  histogramPlot (samples $ exponential 1.5) $ do
    key "exponential"
    normaliseSample .= pdf

  areaStyle . _opacity .= 0.6

  majorGridLineStyle &= do
    _lw .= 0.5
    _lineTexture .= _AC ## opaque grey

main :: IO ()
main = r2AxisMain axis

