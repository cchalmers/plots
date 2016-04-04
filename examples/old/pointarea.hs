{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Plots
import Plots.Axis
import Plots.Types hiding (B)
-- import Plots.Themes
import Plots.Utils

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine

import           Diagrams.Coordinates.Polar

-- import Data.Array
-- import Data.Monoid.Recommend

import Dataset

fillOpacity' = areaStyle . mapped . _opacity

data1 = [(2.7,(1/7 @@ turn)),(3.0,(2/7 @@ turn)),(9.0,(3/7 @@ turn)),(6.2,(4/7 @@ turn)),(7.1,(5/7 @@ turn)),(8.5,(6/7 @@ turn)),(5.3,(7/7 @@ turn))]

data2 = [(5.7,(1/7 @@ turn)),(1.1,(2/7 @@ turn)),(9.0,(3/7 @@ turn)),(4.2,(4/7 @@ turn)),(7.1,(5/7 @@ turn)),(2.5,(6/7 @@ turn)),(6.3,(7/7 @@ turn))]

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do

    pointsPlot' data1 $ do
      doFill .= True
      fillOpacity' .= 0.5

    pointsPlot' data2 $ do
      plotColor .= blue
      fillOpacity' .= 0.5
      doFill    .= True

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



