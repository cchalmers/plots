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

redzone = cycle [sRGB24 255 0 0
  , sRGB24 198  49   49
  , sRGB24 154 65 65
  , sRGB24 117 69 69
  , sRGB24 31 30 30]

corperateB =   cycle [sRGB24 255 127 0
  , sRGB24 166 86  40
  , sRGB24 247 129 191]

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do

    wedgePlotFrom' 7 xDir (2/11 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24  255 127 0
    wedgePlotFrom' 4.3 (rotate (2/11 @@ turn) xDir) (4/11 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .=  sRGB24 166 86  40
    wedgePlotFrom' 8.1 (rotate (6/11 @@ turn) xDir)  (3/11 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 247 129 191
    annularWedgePlotFrom' 3.1  0.1 (rotate (9/11 @@ turn) xDir)  (2/11 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 117 69 69
    annularWedgePlotFrom' 5.7  3.1 (rotate (9/11 @@ turn) xDir)  (2/11 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 154 65 65
    annularWedgePlotFrom' 9.5  5.7 (rotate (9/11 @@ turn) xDir)  (2/11 @@ turn) $ do
       strokeArc .= True
       fillOpacity .= 0.9
       plotColor .= sRGB24 198  49   49

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



