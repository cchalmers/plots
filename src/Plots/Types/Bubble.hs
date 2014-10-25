{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ExistentialQuantification #-}

module Plots.Types.Scatter where
  -- (  scatterPlot
  -- ) where

import Diagrams.Prelude hiding (view)
import Diagrams.Coordinates.Isomorphic
import Data.Default
import Control.Lens hiding ((#), transform, lmap)
import Data.Typeable
import Data.Foldable (Foldable)

import Diagrams.LinearMap

import Plots.Themes
import Plots.Types

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

data GeneralScatterPlot a b v n = forall s. GeneralScatterPlot
  { _gScatterData        :: s
  , _gScatterFold        :: Fold s a
  , _gScatterMaker       :: ThemeEntry b n -> Transformation v n -> (v n -> V2 n) -> T2 n -> [a] -> QDiagram b V2 n Any
  , _gScatterGenericPlot :: GenericPlot b v n
  } deriving Typeable

makeLenses ''GeneralScatterPlot

gScatterGenericPlot :: Lens' (GeneralScatterPlot a b v n) (GenericPlot b v n)
gScatterGenericPlot
  = lens (\(GeneralScatterPlot {..}) -> _gScatterGenericPlot)
         (\sp g -> sp { _gScatterGenericPlot = g })

instance HasGenericPlot (GeneralScatterPlot a b v n) where
  genericPlot = gScatterGenericPlot

type instance B (GeneralScatterPlot a b v n) = b
type instance V (GeneralScatterPlot a b v n) = v
type instance N (GeneralScatterPlot a b v n) = n

instance (Typeable a, Typeable b, Typeable v, TypeableFloat n, Renderable (Path V2 n) b, HasLinearMap v)
    => Plotable (GeneralScatterPlot a b v n) where
  plot _ tv l t2 GeneralScatterPlot {..}
    = _gScatterMaker (_gScatterGenericPlot ^. themeEntry) tv l t2 (_gScatterData ^.. _gScatterFold)


------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

type ScatterPlot b v n = GeneralScatterPlot (Point v n) b v n

mkScatterPlotOf :: (TypeableFloat n, PointLike v n a, Renderable (Path V2 n) b)
  => Fold s a -> s -> ScatterPlot b v n
mkScatterPlotOf f a = GeneralScatterPlot
  { _gScatterData = a
  , _gScatterFold = f . unpointLike
  , _gScatterMaker = scatterMaker
  , _gScatterGenericPlot = def
  }

mkScatterPlot :: (TypeableFloat n, PointLike v n a, Renderable (Path V2 n) b, Foldable f)
  => f a -> ScatterPlot b v n
mkScatterPlot = mkScatterPlotOf folded

scatterMaker :: (TypeableFloat n, Renderable (Path V2 n) b, HasLinearMap v)
  => ThemeEntry b n -> Transformation v n -> (v n -> V2 n) -> T2 n -> [Point v n] -> QDiagram b V2 n Any
scatterMaker theme tv l t2 ps = position $ zip ps' (repeat marker)
  where
    ps'    = transform t2 . lmap l . transform tv $ ps
    marker = applyStyle (theme ^. themeMarkerStyle) (theme ^. themeMarker)

------------------------------------------------------------------------
-- Bubble plot
------------------------------------------------------------------------

type BubblePlot b v n = GeneralScatterPlot (n, Point v n) b v n

mkBubblePlotOf :: (TypeableFloat n, PointLike v n a, Renderable (Path V2 n) b)
  => Fold s (n,a) -> s -> BubblePlot b v n
mkBubblePlotOf f a = GeneralScatterPlot
  { _gScatterData = a
  , _gScatterFold = f . to (over _2 $ view unpointLike)
  , _gScatterMaker = bubbleMaker
  , _gScatterGenericPlot = def
  }

mkBubblePlot :: (TypeableFloat n, PointLike v n a, Renderable (Path V2 n) b, Foldable f)
  => f (n,a) -> BubblePlot b v n
mkBubblePlot = mkBubblePlotOf folded


bubbleMaker :: (TypeableFloat n, Renderable (Path V2 n) b, HasLinearMap v)
  => ThemeEntry b n -> Transformation v n -> (v n -> V2 n) -> T2 n -> [(n, Point v n)] -> QDiagram b V2 n Any
bubbleMaker theme tv l t2 ps = position $ map (`mkBubble` marker) ps'
  where
    ps'    = over (traversed . _2) (transform t2 . lmap l . transform tv) ps
    marker = applyStyle (theme ^. themeMarkerStyle) (theme ^. themeMarker)
    mkBubble (x, p) mark = (p, scale x mark)

