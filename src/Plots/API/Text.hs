{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Text
  ( -- * Text plot
    textPlot
  , textPlot'
  , textPlotL
  ) where

import           Control.Monad.State.Lazy

import           Data.Typeable
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Types
import           Plots.Types.Text
import           Plots.API

------------------------------------------------------------------------
-- Boxplot
------------------------------------------------------------------------

textPlot
  :: (v ~ BaseSpace c,
      RealFloat n,
      Typeable n,
      PointLike v n (V2 n),
      MonadState (Axis b c n) m,
      Plotable (TextPlot n) b,
      v ~ V2)
  => (n,n) -> String -> m ()
textPlot pt a = addPlotable (mkTextPlot pt a)

textPlot'
  :: (v ~ BaseSpace c,
      RealFloat n,
      Typeable n,
      PointLike v n (V2 n),
      MonadState (Axis b c n) m,
      Plotable (TextPlot n) b,
      v ~ V2)
  => (n,n) -> String -> PlotState (TextPlot n) b -> m ()
textPlot' pt a = addPlotable' (mkTextPlot pt a)

textPlotL
  :: (v ~ BaseSpace c,
      RealFloat n,
      Typeable n,
      PointLike v n (V2 n),
      MonadState (Axis b c n) m,
      Plotable (TextPlot n) b,
      F.Foldable f,
      v ~ V2)
  => String -> (n,n) -> String -> m ()
textPlotL l pt a = addPlotableL l (mkTextPlot pt a)

