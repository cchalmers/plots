{-# LANGUAGE FlexibleContexts #-}

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

fillOpacity = barStyle . mapped . _opacity

data1 = [2.3,5.6,7.4,4.3,6.3]

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do

    wedgePlotFrom' 8 (rotate (0/25.9 @@ turn) xDir) (2.3/25.9 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.8
    wedgePlotFrom' 8 (rotate (2.3/25.9 @@ turn) xDir) (5.6/25.9 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.8
    wedgePlotFrom' 8 (rotate (7.9/25.9 @@ turn) xDir) (7.4/25.9 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.8
    wedgePlotFrom' 8 (rotate (15.3/25.9 @@ turn) xDir) (4.3/25.9 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.8
    wedgePlotFrom' 8 (rotate (19.6/25.9 @@ turn) xDir) (6.3/25.9 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.8

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



