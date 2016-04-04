{-# LANGUAGE FlexibleContexts #-}

import Plots

import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine


myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do

  vectorFieldPlot (map vectorField loc1) loc1 arrowOpts

  vectorFieldPlot (map vectorField2 loc1) loc1 arrowOpts2

  yMin ?= 0

point1  = (0.2, 0.2)
vector1 = r2 (0.2, 0.2)

loc1 = [(x, y) | x <- [0.5,0.7 .. 4.3], y <- [0.5,0.7 .. 4.3]]

vectorField (x, y) = r2 ((sin y)/4, (sin (x+1))/4)

vectorField2 (x, y) = r2 ((cos y)/4, (cos (x+1))/4)

arrowOpts = ( with & arrowHead .~ tri & headLength .~ global 5 & headTexture .~ solid blue)
arrowOpts2 = ( with & arrowHead .~ tri & headLength .~ global 5 & headTexture .~ solid red)

make :: Diagram B -> IO ()
make = renderRasterific "test.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis

