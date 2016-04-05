{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import Dataset

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  histogramPlot sepalLength $ do
     key "sepal length"
     plotColor .= blue
     areaStyle . _opacity .= 0.5

  yMin .= Just 0

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis
