{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Points
  ( -- * Points plot
    pointsPlot
  , pointsPlot'
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable as F

import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Types
import           Plots.API
import           Plots.Types.Points

------------------------------------------------------------------------
-- Points plot
------------------------------------------------------------------------

-- $ points plot
-- Points plot display data as scatter (dots) on polar co-ord.
-- Points plots have the following lenses:
--
-- @
-- * 'doFill' :: 'Lens'' ('BoxPlot' v n) 'Bool' - False
-- @
--
-- | Add a 'PointsPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = polarAxis ~&
--     pointsPlot data1
-- @
--
-- === __Example__
--
-- <<plots/points.png#diagram=points&width=300>>
--
-- @
--
-- myaxis :: Axis B Polar Double
-- myaxis = polarAxis &~ do
--    pointsPlot mydata1
--    pointsPlot mydata2
--    pointsPlot mydata3
--
-- @

pointsPlot
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPointsPlot n) b,
      RealFloat n)
  => [(n,Angle n)] -> m ()
pointsPlot ds = addPlotable (mkPointsPlot ds)

-- | Make a 'PointsPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = polarAxis &~ do
--     pointsPlot' pointData1 $ do
--       addLegendEntry "data 1"
--       doFill .= True
-- @

pointsPlot'
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPointsPlot n) b,
      RealFloat n)
  => [(n,Angle n)] -> PlotState (GPointsPlot n) b -> m ()
pointsPlot' ds = addPlotable' (mkPointsPlot ds)



