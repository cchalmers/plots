{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Scatter
  ( -- * Scatter plot
    ScatterPlot
  , mkScatterPlot
  , mkScatterPlotOf
  , _ScatterPlot

    -- * Bubble plot
  , BubblePlot
  , mkBubblePlot
  , mkBubblePlotOf

    -- * General scatter plot
  , GScatterPlot
  , mkGScatterPlot
  , mkGScatterPlotOf

    -- * Scatter plot lenses
  , scatterTransform
  , scatterStyle
  , connectingLine
  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Data.Foldable                   (Foldable)
import           Data.Typeable
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (view)

import           Plots.Themes
import           Plots.Types

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

data GScatterPlot v n a = forall s. GScatterPlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sTr   :: Maybe (a -> T2 n)
  , sSty  :: Maybe (a -> Style V2 n)
  , cLine :: Bool
  } deriving Typeable

type instance V (GScatterPlot v n a) = v
type instance N (GScatterPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GScatterPlot v n a) where
  getEnvelope GScatterPlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GScatterPlot V2 n a) b where
  renderPlotable _ t GScatterPlot {..} pp =
      foldMapOf sFold mk sData # applyMarkerStyle pp
   <> if cLine
        then fromVertices (toListOf (sFold . to sPos) sData)
               # transform t
               # applyLineStyle pp
        else mempty
    where
      mk a = marker # moveTo (papply t $ sPos a)
                    # maybe id (transform  . ($ a)) sTr
                    # maybe id (applyStyle . ($ a)) sSty
      marker = pp ^. plotMarker

  defLegendPic GScatterPlot {..} pp =
    pp ^. plotMarker
      & applyMarkerStyle pp

_ScatterPlot :: (Plotable (ScatterPlot v n) b, Typeable b)
             => Prism' (Plot b v n) (ScatterPlot v n)
_ScatterPlot = _Plot

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

type ScatterPlot v n = GScatterPlot v n (Point v n)

-- | Make a scatter plot.
mkScatterPlot :: (PointLike v n p, Foldable f, Num n)
              => f p -> ScatterPlot v n
mkScatterPlot = mkScatterPlotOf folded

-- | Make a scatter plot using the given fold.
mkScatterPlotOf :: (PointLike v n p, Num n)
                => Fold s p -> s -> ScatterPlot v n
mkScatterPlotOf f a = GScatterPlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sTr   = Nothing
  , sSty  = Nothing
  , cLine = False
  }

------------------------------------------------------------------------
-- Bubble plot
------------------------------------------------------------------------

type BubblePlot v n = GScatterPlot v n (n, Point v n)

mkBubblePlotOf :: (PointLike v n p, Fractional n)
               => Fold s (n,p) -> s -> BubblePlot v n
mkBubblePlotOf f a = GScatterPlot
  { sData = a
  , sFold = f . to (over _2 $ view unpointLike)
  , sPos  = snd
  , sTr   = Just (scaling . fst)
  , sSty  = Nothing
  , cLine = False
  }

mkBubblePlot :: (PointLike v n p, Foldable f, Fractional n)
             => f (n,p) -> BubblePlot v n
mkBubblePlot = mkBubblePlotOf folded

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

mkGScatterPlotOf :: (PointLike v n p, Fractional n)
                 => Fold s a -> s -> (a -> p) -> GScatterPlot v n a
mkGScatterPlotOf f a pf = GScatterPlot
  { sData = a
  , sFold = f
  , sPos  = view unpointLike . pf
  , sTr   = Nothing
  , sSty  = Nothing
  , cLine = False
  }

mkGScatterPlot :: (PointLike v n p, Foldable f, Fractional n)
               => f a -> (a -> p) -> GScatterPlot v n a
mkGScatterPlot = mkGScatterPlotOf folded

------------------------------------------------------------------------
-- Scatter plot lenses
------------------------------------------------------------------------

class HasScatter a v n d | a -> v n, a -> d where
  scatter :: Lens' a (GScatterPlot v n d)

  scatterTransform :: Lens' a (Maybe (d -> T2 n))
  scatterTransform = scatter . lens sTr (\sp t -> sp {sTr = t})

  -- | Change the style for a scatter plot, given the data entry.
  --
  -- @@@
  -- mybubbleplot & scatterStyle     ?~ mkAttr . transparency
  --              & scatterTransform .~ Nothing
  -- @@@
  scatterStyle :: Lens' a (Maybe (d -> Style V2 n))
  scatterStyle = scatter . lens sSty (\sp sty -> sp {sSty = sty})


  connectingLine :: Lens' a Bool
  connectingLine = scatter . lens cLine (\s b -> (s {cLine = b}))

instance HasScatter (GScatterPlot v n d) v n d where
  scatter = id

instance HasScatter (PropertiedPlot (GScatterPlot v n d) b) v n d where
  scatter = _pp
