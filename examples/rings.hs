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

grayScale =   cycle [sRGB24 33 33 33
  , sRGB24 77  77   77
  , sRGB24 122 122 122
  , sRGB24 163 163 163
  , sRGB24 191 191 191
  , sRGB24 227 227 227]

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do

    annularWedgePlotFrom' 9  7 xDir (1 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 227 227 227
    annularWedgePlotFrom' 7  4.3 xDir (1 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .=  sRGB24 163 163 163
    annularWedgePlotFrom' 4.3  3.1 xDir (1 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 77  77   77
    annularWedgePlotFrom' 3.1  0.1 xDir (1 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 191 191 191

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



