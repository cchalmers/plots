{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
import Plots.Themes
import Plots.Utils

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import           Diagrams.Coordinates.Polar

import Data.Array
import Data.Monoid.Recommend

import Dataset

data1 = [(2.0,(1/3 @@ turn)),(3.0,(1/2 @@ turn)),(5.0,(4/5 @@ turn)),(6.0,(2/3 @@ turn)),(7.0,(1/6 @@ turn)),(8.0,(1/3 @@ turn)),(5.4,(2/3 @@ turn)),(7.1,(4/6 @@ turn)),(8.2,(3/5 @@ turn)),(9.0,(1/3 @@ turn))]

data2 = [ (x/2.0, y)| (x,y) <- data1]

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do

    pointsPlot data1
    pointsPlot data2

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



