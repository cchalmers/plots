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

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do

    wedgePlotFrom' 10 (rotate (1/2 @@ turn) xDir) (1/4 @@ turn) $ do
       strokeArc .= True
       plotColor .= orange
       fillOpacity .= 0.9
    annularWedgePlotFrom' 9 7 (rotate (1/3 @@ turn) xDir) (1/3 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.7
    annularWedgePlotFrom' 7 4 (rotate (7/9 @@ turn) xDir) (2/3 @@ turn) $ do
       fillOpacity .= 0.8

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



