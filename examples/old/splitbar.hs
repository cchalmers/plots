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
import Data.Foldable

data1 = [2.5, 4.4, 5.9]
data2 = [0.2, 2.1, 4.8]
data3 = [0.8, 1.1, 3.7, 4.1, 3.2]
data4 = [1.2]
data5 = [1.7, 1.3, 2.4, 2.5]
data6 = [4.7, 7.3, 5.1]

legend = ["first", "second", "third"]

mydata = [data1 , data2, data3, data4, data5, data6]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  noGridLines
  noMinorTicks
    
  barPlotSplitMultiC mydata redzone legend 0.5

  xMax .= Commit 7
  yMin .= Commit 0
  
  axisTickLabels . _x . tickLabelFun .= stringLabels xs
     where xs = ["a", "b", "c", "d", "e", "f"]

corperateA =   cycle [sRGB24 77 221 152
  , sRGB24 152 77 221
  , sRGB24 221 152 77
  , sRGB24 122 33  134
  , sRGB24 11  23  144]

grayScale =   cycle [sRGB24 33 33 33
  , sRGB24 77  77   77
  , sRGB24 122 122 122
  , sRGB24 163 163 163
  , sRGB24 191 191 191
  , sRGB24 227 227 227]

corperateB =   cycle [sRGB24 255 127 0
  , sRGB24 166 86  40
  , sRGB24 247 129 191]

redzone = cycle [sRGB24 255 0 0
  , sRGB24 198  49   49
  , sRGB24 154 65 65
  , sRGB24 117 69 69
  , sRGB24 31 30 30]

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



