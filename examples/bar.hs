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


mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  noGridLines
  noMinorTicks
  
  barPlot (1.0,3.2) 0.5

  barPlot' (2.0,5.2) 0.5 $ do
    addLegendEntry "dragon" 
    plotColor .= blue
    strokeEdge .= False

  barPlotNormal 3 (0.8, 2.1, 3.4, 5.6) 0.7

  barPlotStacked 4 (0.2, 0.6, 2.1) 0.5

  barPlotRatio 5 (0.6, 1.6, 3.6) 0.3

-- take a [colour] and data as [Double]
  barPlotSplit 6 (0.8, 3.4, 5.6) 0.7
  
  xMax .= Commit 7
  yMin .= Commit 0
  
  axisTickLabels . _x . tickLabelFun .= stringLabels xs
     where xs = ["simple", "withLegend", "normal", "stacked", "ratio", "split"]

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



