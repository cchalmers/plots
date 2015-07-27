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


mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1),(1,3), (2,5.5), (3.2, 6), (5.5, 6.1),(1,3), (2,5.5), (3.2, 6), (3.5, 6.1),(1,3), (2,5.5), (3.2, 6), (3.5, 6.1),(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  
  histogramPlot mydata1  
  
  yMin .= Commit 0

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



