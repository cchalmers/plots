{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
import Plots.Themes
import Plots.Utils

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import Data.Array
import Data.Monoid.Recommend

import Dataset

alldata = zip (zip petalLength petalWidth) species
  
mydata1 = foobarsingle alldata "setosa"
mydata2 = foobarsingle alldata "versicolor"
mydata3 = foobarsingle alldata "virginica"

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

    textPlot' (5.1,5.0) "test dragon" $ do
       plotColor .= darkblue 

 
make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



