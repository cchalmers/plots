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

import Data.Array
import Data.Monoid.Recommend

import Dataset

alldata = zip (zip petalLength petalWidth) species
  
mydata1 = foobarsingle alldata "setosa"
mydata2 = foobarsingle alldata "versicolor"
mydata3 = foobarsingle alldata "virginica"

fillOpacity = barStyle . mapped . _opacity

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
--    smoothPlot mydata1

--    densityPlot' mydata1 $ do
--      fillArea .= True
--      addLegendEntry "mydata1"

    boxPlot' (fst mydata1) $ do
      plotColor .= teal
      fillOpacity .= 0.5
      addLegendEntry (snd mydata1)
      
    boxPlot' (fst mydata2) $ do
      plotColor .= orange
      fillOpacity .= 0.5
      addLegendEntry (snd mydata2)

    boxPlot' (fst mydata3) $ do
      plotColor .= purple
      fillOpacity .= 0.5
      addLegendEntry (snd mydata3)

    scatterPlot' (fst mydata1) $ do
      plotColor .= teal

    scatterPlot' (fst mydata2) $ do
      plotColor .= orange

    scatterPlot' (fst mydata3) $ do
      plotColor .= purple
 
make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



