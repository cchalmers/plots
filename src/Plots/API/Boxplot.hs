{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Boxplot
  (  -- * Boxplot
     boxPlot
   , boxPlot'
   , boxPlotL

     -- * Fold variant boxplot
   , boxPlotOf
   , boxPlotOf'
   , boxPlotOfL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Types
import           Plots.API
import           Plots.Types.Boxplot

------------------------------------------------------------------------
-- Boxplot
------------------------------------------------------------------------

-- $ boxplot
-- Box plots display data as boxplot. There are several representations
-- for boxplot plots for extra parameters. Box plots have the
-- following lenses:
--
-- @
-- * 'fillBox' :: 'Lens'' ('BoxPlot' v n) 'Bool' - False
-- @

-- | Add a 'BoxPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     boxPlot data1
-- @
--
-- === __Example__
--
-- <<plots/boxplot.png#diagram=boxplot&width=300>>
--
-- @
-- mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- mydata2 = mydata1 & each . _1 *~ 0.5
-- mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--    boxPlot mydata1
--    boxPlot mydata2
--    boxPlot mydata3
-- @

boxPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
boxPlot d = addPlotable (mkBoxPlot d)

-- | Make a 'BoxPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     boxPlot' pointData1 $ do
--       fillBox .= True
--       addLegendEntry "data 1"
-- @

boxPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (BoxPlot v n) b -> m ()
boxPlot' d = addPlotable' (mkBoxPlot d)

-- | Add a 'BoxPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     boxPlotL "blue team" pointData1
--     boxPlotL "red team" pointData2
-- @


boxPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
boxPlotL l d = addPlotableL l (mkBoxPlot d)



-- Fold variants

boxPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
boxPlotOf f s = addPlotable (mkBoxPlotOf f s)

boxPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (BoxPlot v n) b -> m ()
boxPlotOf' f s = addPlotable' (mkBoxPlotOf f s)

boxPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
boxPlotOfL l f s = addPlotableL l (mkBoxPlotOf f s)

