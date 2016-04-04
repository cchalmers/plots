{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
import Plots.Style
import Plots.Utils

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import Data.Array
import Data.Monoid.Recommend

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

    textPlot' (5.1,5.0) "test dragon"
    -- textPlot' (5.1,5.0) "test dragon" $ do
       -- plotColor .= darkblue


make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis
