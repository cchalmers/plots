{-# LANGUAGE FlexibleContexts #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Data.Typeable

mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  linePlot'  mydata1
  linePlot mydata2 $ do
    key "data 2"
    plotColor .= black
    dotsonPoint .= True

  linePlot mydata3 $ key "data 3"

  axisPlots . each . _LinePlot' . dotsonPoint .= True

_LinePlot' :: (Plotable (LinePlot v n) b, Typeable b)
           => Traversal' (DynamicPlot b v n) (LinePlot v n)
_LinePlot' = _DynamicPlot . rawPlot

main :: IO ()
main = r2AxisMain myaxis
