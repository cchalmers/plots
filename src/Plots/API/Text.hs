{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Text
  ( textPlot
  , textPlot'
  , textPlotL
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

import           Plots.Types.Text
import           Plots.API


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

