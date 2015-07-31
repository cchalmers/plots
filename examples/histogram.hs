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

fillOpacity = barStyle . mapped . _opacity

mydata1 = [(0.3,3), (0.5,5.5), (0.8, 6), (1.1, 6.1),(1.3,3), (1.4,5.5), (1.6, 6), (1.7, 6.1),(2.1,3), (2.5,5.5), (3.2, 6), (3.5, 6.1),(2.4,3), (2.6,5.5), (3.2, 6), (3.5, 6.1),(2.6,3), (2.7,5.5), (3.2, 6), (3.1, 6.1)]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  
  histogramPlot' mydata1 $ do 
     addLegendEntry "histogram"
     plotColor .= blue
     fillOpacity .= 0.5
     
  yMin .= Commit 0

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis
