{-# LANGUAGE FlexibleContexts #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Dataset

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  histogramPlot sepalLength $ do
     key "sepal length"
     plotColor .= blue
     areaStyle . _opacity .= 0.5

  yMin .= Just 0

main :: IO ()
main = r2AxisMain myaxis
