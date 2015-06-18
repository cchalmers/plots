{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  scatterPlotL "data 1" mydata1
  scatterPlotL "data 2" mydata2
  scatterPlotL "data 3" mydata3

  axisPlots . each . _ScatterPlot' . connectingLine .= True

_ScatterPlot' :: Plotable (ScatterPlot v n) b => Traversal' (Plot' b v n) (ScatterPlot v n)
_ScatterPlot' = _Plot'

make :: Diagram B -> IO ()
make = renderRasterific "examples/scatter.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis

