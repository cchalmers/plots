{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
import Plots.Themes

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import Data.Monoid.Recommend

data1 = (1.2, 3.4, 4.2, 5.4)
data2 = (0.2, 2.1, 3.2, 4.8)
data3 = (0.8, 1.1, 2.2, 3.2)
data4 = (2.5, 4.4, 5.2, 5.9)
data5 = (1.7, 2.6, 4.2, 4.7)

legend = ("first", "first", "first", "first")

mydata = (data1 , data2, data3, data4, data5)

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  noGridLines
  noMinorTicks
    
  barPlotNormalMultiC mydata corperateC legend 0.7

  xMax .= Commit 6
  yMin .= Commit 0
  
  axisTickLabels . _x . tickLabelFun .= stringLabels xs
     where xs = ["a", "b", "c", "d", "e"]


corperateC =   (sRGB24 27  158 119
  , sRGB24 217 95  2
  , sRGB24 117 112 179
  , sRGB24 231 41  138)



make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



