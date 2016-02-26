{-# LANGUAGE FlexibleContexts #-}

import Plots

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

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

    scatterPlot (fst mydata1) $ plotColor .= teal
    scatterPlot (fst mydata2) $ plotColor .= orange
    scatterPlot (fst mydata3) $ plotColor .= purple

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 1000) . frame 20

main :: IO ()
main = make $ renderAxis myaxis



