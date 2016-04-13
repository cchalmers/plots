{-# LANGUAGE FlexibleContexts #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

mydata :: [V2 Double]
mydata = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
   ribbonPlot (mydata ++ zero_y (reverse mydata)) $ do
     key "ribbon test"
     plotColor  .= red

     strokeEdge .= False

-- Turn a data set into an area data set by adding the same points in reverse order with a 0 y to complete the path
areaise :: [V2 Double] -> [V2 Double]
areaise xs = xs ++ zero_y (reverse xs)

zero_y :: [V2 Double] -> [V2 Double]
zero_y = set (each . _y) 0

main :: IO ()
main = r2AxisMain myaxis
