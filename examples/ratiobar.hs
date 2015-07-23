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

data1 = (1.2, 4.2, 5.4)
data2 = (0.2, 2.1, 4.8)
data3 = (0.8, 1.1, 3.2)
data4 = (2.5, 4.4, 5.9)
data5 = (1.7, 4.2, 4.7)

legend = ("first", "second", "third")

mydata = (data1 , data2, data3, data4, data5)

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  noGridLines
  noMinorTicks
    
  barPlotRatioMultiC mydata corperateB legend 0.7

  xMax .= Commit 6
  yMin .= Commit 0
  yMax .= Commit 1
  
  axisTickLabels . _x . tickLabelFun .= stringLabels xs
     where xs = ["a", "b", "c", "d", "e"]


corperateB =   (sRGB24 77 77 77
  , sRGB24 152 152 152
  , sRGB24 221 221 221)

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



