{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)

import Data.List

import Dataset
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
            linerangevPlotL "vertical linerange" (1,3) 1
            errorbarvPlotwithPoint (3.2, 6) 0.2 0.4
            errorbarhPlot (3.5, 6.1) 0.5 0.5
            crossbarvPlotwithPoint (2, 1) 0.2 0.4
            crossbarhPlot (1.2, 4) 0.2 0.4
            boxplotvPlot (0.5, 1.2) 0.2 0.4 0.7


_LinePlot' :: Plotable (LinePlot v n) b => Traversal' (Plot' b v n) (LinePlot v n)
_LinePlot' = _Plot'

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis

foo1 = ([(111.0,0.1),(140.0,1.2),(150.0,2.3)],"typeA")
foo2 = ([(155.0,3.5),(167.0,5.1),(200.0,6.4),(211.0,7.5)],"typeB")
foo3 = ([(191.0,5.8),(233.0,8.5),(250.0,9.1),(270.0,9.6)],"typeC")







