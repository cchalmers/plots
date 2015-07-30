{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
import Plots.Themes

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import Data.Array
import Data.Monoid.Recommend


mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
--    smoothPlot mydata1

--    densityPlot' mydata1 $ do
--      fillArea .= True
--      addLegendEntry "mydata1"
   
    boxPlot mydata1
    boxPlot mydata2
    boxPlot mydata3

    scatterPlot mydata1
    scatterPlot mydata2
    scatterPlot mydata3


make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



