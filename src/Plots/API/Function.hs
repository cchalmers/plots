{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Function
  ( parametricPlot
  , parametricRangePlot
  -- ** Parametric plot

  , parametricPlot'
  , parametricRangePlot'

  , parametricPlotL
  , parametricRangePlotL

  , abLinePlot
  , hLinePlot
  , vLinePlot

  , vectorPlot
  , vectorPointPlot
  , vectorPointPlot'
  , vectorPointPlot''
  , vectorPointPlotL
  , vectorFieldPlot
  -- ** Mesh plot
  -- , meshPlot
  -- , surfacePlot
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable
import qualified Data.Foldable as F
import           Data.List
import           Data.Function

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Linear

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes

import           Plots.Types.Function
import           Plots.API

------------------------------------------------------------------------
-- Vector Plot
------------------------------------------------------------------------

vectorPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> m ()
vectorPlot f = addPlotable (mkVectorPlot f)

vectorPointPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> (n, n)  -> m ()
vectorPointPlot f d = addPlotable (mkVectorPointPlot f d)

vectorPointPlot'
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> (n, n) -> PlotState (VectorPlot v n) b -> m ()
vectorPointPlot' f d = addPlotable' (mkVectorPointPlot f d)

vectorPointPlot''
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> (n, n) -> ArrowOpts n -> m ()
vectorPointPlot'' f d opts = addPlotable' (mkVectorPointPlot f d) $ do
                              setArrowOpts .= opts

vectorPointPlotL
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  String -> v n -> (n, n) -> m ()
vectorPointPlotL l f d = addPlotableL l (mkVectorPointPlot f d)

vectorFieldPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  [v n] -> [(n, n)] -> ArrowOpts n -> m ()
vectorFieldPlot vs ps opts = F.for_ (zip vs ps) $ \x -> vectorPointPlot'' (fst x) (snd x) opts

------------------------------------------------------------------------
-- Parametric Plot
------------------------------------------------------------------------

parametricPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> m ()
parametricPlot f = addPlotable (mkParametricPlot f)

parametricRangePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> (n ,n) -> m ()
parametricRangePlot f d = addPlotable (mkParametricRangePlot f d)

parametricPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> PlotState (ParametricPlot v n) b -> m ()
parametricPlot' f = addPlotable' (mkParametricPlot f)

parametricRangePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> (n ,n) -> PlotState (ParametricPlot v n) b -> m ()
parametricRangePlot' f d = addPlotable' (mkParametricRangePlot f d)

parametricPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => String -> (n -> p) -> m ()
parametricPlotL l f = addPlotableL l (mkParametricPlot f)

parametricRangePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => String -> (n -> p) -> (n ,n) -> m ()
parametricRangePlotL l f d = addPlotableL l (mkParametricRangePlot f d)

-------------------------------------------------------------------------------
-- Line
-------------------------------------------------------------------------------

abLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> n -> (n ,n) -> m ()
abLinePlot slope intercept d = addPlotable (mkParametricRangePlot (createABLine slope intercept) d)

hLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> (n ,n) -> m ()
hLinePlot intercept d = addPlotable (mkParametricRangePlot (createHLine intercept) d)

vLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> (n ,n) -> m ()
vLinePlot intercept d = addPlotable (mkParametricRangePlot (createVLine intercept) d)
