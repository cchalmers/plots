{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Smooth
  (  -- * Smooth Plot
     smoothPlot
   , smoothPlot'
   , smoothPlotL
     -- * Fold variant smooth plot
   , smoothPlotOf
   , smoothPlotOf'
   , smoothPlotOfL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Types
import           Plots.API
import           Plots.Types.Smooth

------------------------------------------------------------------------
-- Smooth Plot
------------------------------------------------------------------------

-- $ smoothplot
-- smooth plots display data as various functions. There are
-- several representations for smooth plots for extra parameters.
-- Smooth plots have the following lenses:
--
-- @
-- * 'sLine' :: 'Lens'' ('BoxPlot' v n) 'Bool' - False
-- @

-- | Add a 'SmoothPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     smoothPlot data1
-- @
--
-- === __Example__
--
-- <<plots/smoothsimple.png#diagram=smoothsimple&width=300>>
--
-- @
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--    smoothPlot mydata1
--    smoothPlot mydata2
--    smoothPlot mydata3
-- @

smoothPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
smoothPlot d = addPlotable (mkSmoothPlot d)

-- | Make a 'SmoothPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     smoothPlot' pointData1 $ do
--       sLine .= False
--       addLegendEntry "data 1"
-- @

smoothPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (SmoothPlot v n) b -> m ()
smoothPlot' d = addPlotable' (mkSmoothPlot d)

-- | Add a 'SmoothPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     smoothPlotL "blue team" pointData1
--     smoothPlotL "red team" pointData2
-- @

smoothPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
smoothPlotL l d = addPlotableL l (mkSmoothPlot d)

-- fold variant

smoothPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
smoothPlotOf f s = addPlotable (mkSmoothPlotOf f s)

smoothPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (SmoothPlot v n) b -> m ()
smoothPlotOf' f s = addPlotable' (mkSmoothPlotOf f s)

smoothPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
smoothPlotOfL l f s = addPlotableL l (mkSmoothPlotOf f s)
