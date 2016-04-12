{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.Backend.Rasterific.CmdLine
import Control.Monad.State

-- # Plot properties
--
-- Every plot has an assosiated 'PlotProperties'. This contains things
-- like style, name, legend enteries etc. A plot is combined with it's
-- properties with a 'PropertiedPlot'. This contains lots of instances
-- make it easy to modify with lenses.
--
-- The primed versions of adding plots allow the edit the
-- 'PropertiedPlot' state with do notation:
--
-- @
-- scatterPlot
--   :: [P2 Double]                         -- data points
--   -> PlotState (ScatterPlot V2 Double) B -- state to alter the plot options
--   -> AxisState B V2 Double               -- state that adds plot to axis
-- @
--
-- The 'PlotState' here can accept generic plot code like
-- @addLegendEntry@, @plotColor@ etc. as well as code specific to that
-- plot (like 'connectingLine').

-- # Generic scatter plot
--
-- The generic scatter plot is general data type for scatter plots. It
-- basically consists of
--
-- data: sData :: [a]
-- data to point in space: sPos :: (a -> P2 Double)
-- data to transform of marker: sTr :: (a -> T2 Double)
-- data to style of marker: sTr :: (a -> Style V2 Double)
-- whether to draw connecting line between points :: cLing
--
-- The point style and transform are optional. With this data type you
-- can make scatter / bubble / colourDependant plots.


-- data with extra z coordinate
mydata1 = [V3 1.3 4 5, V3 2 5.5 4, V3 3.2 6 3, V3 3.5 6.1 2]
mydata2 = mydata1 & each . _x *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  -- get the colour map of the axis
  cm <- use axisColourMap

  gscatterPlot mydata1 (view _xy) $ do
    key "data 1"
    plotColor .= black

    -- scale the point according to the z coordinate
    scatterTransform .= \x -> scaling (x ^. _z)

    scatterStyle .= \x ->
          -- index for colour map (divide by 5 because this is the max z value)
      let i = toRational (x ^. _z) / 5
          -- colour taken from colour map
          c = cm ^. ixColour i
      in  mempty # fcA c
                 # lw none

    connectingLine .= True
  scatterPlotOf (each . _xy) mydata2 $ key "data 2"
  scatterPlot mydata3 $ key "data 3"

  -- add a connecting line for anything matching the '_ScatterPlot' traversal
  connectingLine .= True

-- _ScatterPlot' :: Plotable (ScatterPlot v n) b => Traversal' (Plot' b v n) (ScatterPlot v n)
-- _ScatterPlot' = _Plot'

main :: IO ()
main = mainWith $ renderAxis myaxis

------------------------------------------------------------------------

-- gscatterPlot' :: (v ~ BaseSpace c, PointLike v n p, Plotable (GScatterPlot v n a) b, Foldable f)
--              => f a -> (a -> p) -> PlotState (GScatterPlot v n a) b -> AxisState b c n
-- gscatterPlot' d f = addPlotable' (mkGScatterPlot d f)
