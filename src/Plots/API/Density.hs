{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Density
  (  -- * Density plot
     densityPlot
   , densityPlot'
   , densityPlotL

     -- * Fold variant density plot
   , densityPlotOf
   , densityPlotOf'
   , densityPlotOfL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis

import           Plots.Types

import           Plots.Types.Density
import           Plots.API

------------------------------------------------------------------------
-- Density Plot
------------------------------------------------------------------------

-- $ density plot
-- Density plots display data as average x density of given points,
-- Box plots have the following lenses:
--
-- @
-- * 'fillArea' :: 'Lens'' ('DensityPlot' v n) 'Bool' - False
-- @

-- | Add a 'DenistyPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     densityPlot data1
-- @
--
-- === __Example__
--
-- <<plots/density.png#diagram=density&width=300>>
--
-- @
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--
--     densityPlotL mydata1
--     densityPlotL mydata2
--     densityPlotL mydata3
-- @

densityPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
densityPlot d = addPlotable (mkDensityPlot d)

-- | Make a 'DensityPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     densityPlot' pointData1 $ do
--       fillArea .= True
--       addLegendEntry "data 1"
-- @

densityPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (DensityPlot v n) b -> m ()
densityPlot' d = addPlotable' (mkDensityPlot d)

-- | Add a 'DensityPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     densityPlotL "blue team" pointData1
--     densityPlotL "red team" pointData2
-- @

densityPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
densityPlotL l d = addPlotableL l (mkDensityPlot d)

-- fold variant 

densityPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
densityPlotOf f s = addPlotable (mkDensityPlotOf f s)

densityPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (DensityPlot v n) b -> m ()
densityPlotOf' f s = addPlotable' (mkDensityPlotOf f s)

densityPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
densityPlotOfL l f s = addPlotableL l (mkDensityPlotOf f s)
