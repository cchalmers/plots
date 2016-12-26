{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.Scatter
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A scatter plot is a type of mathematical diagram using Cartesian
-- coordinates to display values for typically two variables for a set
-- of data.
--
-- <<diagrams/src_Plots_Types_Scatter_scatterExample.svg#diagram=scatterExample&height=350>>
--
-- (see 'scatterPlot' example for code to make this plot)
--
----------------------------------------------------------------------------
module Plots.Types.Scatter
  ( -- * Scatter plot
    ScatterPlot

    -- * Scatter plot lenses
  , ScatterOptions
  , HasScatterOptions (..)
  , HasConnectingLine (..)

    -- * Basic scatter plot
    -- ** Add plots to the axis
  , scatterPlot
  , scatterPlot'
  , scatterPlotOf
  , scatterPlotOf'

    -- * Scatter options
  , scatterOptions

    -- * Bubble plots
  , bubblePlot
  , bubblePlot'
  , bubblePlotOf
  , bubblePlotOf'

    -- ** Bubble options
  , BubbleOptions
  , bubbleOptions
  , bubbleTransform
  , bubbleStyle

    -- ** General scatter plot
  , gscatterPlot
  , gscatterOptionsFor

    -- * Low level construction
  , mkScatterOptions
  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable                   as F
import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (view)

import           Plots.Axis
import           Plots.Style
import           Plots.Types

-- config
-- > import Plots

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

-- | A general data type for scatter plots. Allows storing different
--   types of data as well as allowing transforms depending on the data.
data ScatterPlot v where
  ScatterPlot :: Typeable a => ScatterOptions v a -> ScatterPlot v
  deriving Typeable

type instance V (ScatterPlot v) = v
type instance N (ScatterPlot v) = Double

-- | A general data type for scatter plots. Allows storing different
--   types of data as well as allowing transforms depending on the data.
data ScatterOptions v a = ScatterOptions
  { oData :: [a]
  , oPos  :: a -> Point v Double
  , oTr   :: a -> Transformation v Double
  , oSty  :: a -> Style v Double
  , oLine :: Bool
  } deriving Typeable

type instance V (ScatterOptions v a) = v
type instance N (ScatterOptions v a) = Double

instance Metric v => Enveloped (ScatterPlot v) where
  getEnvelope (ScatterPlot (ScatterOptions {..})) = getEnvelope (map oPos oData)

instance Plotable (ScatterPlot V2) where
  renderPlotable s sty (ScatterPlot (ScatterOptions {..})) =
    markers <> line
    where
      markers = F.foldMap mk oData # applyMarkerStyle sty
      --
      mk a = marker # transform (oTr a)
                    # applyStyle (oSty a)
                    # moveTo (specPoint s $ oPos a)
      marker = sty ^. plotMarker
      --
      line
        | not oLine = mempty
        | otherwise = fromVertices points # applyLineStyle sty
      points = map (specPoint s . oPos) oData

  defLegendPic sty (ScatterPlot (ScatterOptions {..})) =
    sty ^. plotMarker
      & applyMarkerStyle sty

------------------------------------------------------------------------
-- Generating scatter options
------------------------------------------------------------------------

-- | Low level construction of 'ScatterOptions'.
mkScatterOptions
  :: (PointLike v Double p, F.Foldable f)
  => f a
  -> (a -> p)
  -> ScatterOptions v a
mkScatterOptions xs pf = ScatterOptions
  { oData = F.toList xs
  , oPos  = view unpointLike . pf
  , oTr   = mempty
  , oSty  = const mempty
  , oLine = False
  }

------------------------------------------------------------------------
-- Scatter plot lenses
------------------------------------------------------------------------

-- | Class of things that have a 'LensLike' for a 'ScatterPlot' \'s
--   connecting line.
class HasConnectingLine f a where
  -- | 'LensLike' onto whether the scatter plot should have a connecting
  --   line between points. If the line is present, it uses the
  --   'lineStyle' from the 'PlotStyle'.
  connectingLine :: Functor f => LensLike' f a Bool

instance HasConnectingLine f (ScatterOptions v a) where
  connectingLine = lens oLine (\o b -> o {oLine = b})

instance HasConnectingLine f (ScatterPlot v) where
  connectingLine f (ScatterPlot o@(ScatterOptions {..}))
    = f oLine <&> \b -> ScatterPlot o {oLine = b}

instance HasConnectingLine f p => HasConnectingLine f (Plot p) where
  connectingLine = rawPlot . connectingLine

instance (Applicative f, Typeable v)
    => HasConnectingLine f (DynamicPlot v) where
  connectingLine = (dynamicPlot :: Traversal' (DynamicPlot v) (Plot (ScatterPlot v)))
                 . connectingLine

instance (Applicative f, Typeable v)
    => HasConnectingLine f (StyledPlot v) where
  connectingLine = (styledPlot :: Traversal' (StyledPlot v) (ScatterPlot v))
                 . connectingLine

instance (Settable f, Typeable (BaseSpace c))
    => HasConnectingLine f (Axis c) where
  connectingLine = finalPlots . connectingLine

-- Options -------------------------------------------------------------

class HasScatterOptions f a d where
  -- | Lens onto the 'ScatterOptions' for a general scatter plot.
  gscatterOptions :: LensLike' f a (ScatterOptions (V a) d)

  -- | Apply a transform to the markers using the associated data.
  scatterTransform :: Functor f => LensLike' f a (d -> Transformation (V a) Double)
  scatterTransform = gscatterOptions . lens oTr (\o tr -> o {oTr = tr})

  -- | Apply a style to the markers using the associated data.
  scatterStyle :: Functor f => LensLike' f a (d -> Style (V a) Double)
  scatterStyle = gscatterOptions . lens oSty (\o sty -> o {oSty = sty})

  -- | Change the position of the markers depending on the data.
  scatterPosition :: Functor f => LensLike' f a (d -> Point (V a) Double)
  scatterPosition = gscatterOptions . lens oPos (\o pos -> o {oPos = pos})

instance d ~ d' => HasScatterOptions f (ScatterOptions v d) d' where
  gscatterOptions = id

instance (Applicative f, Typeable v, Typeable d)
    => HasScatterOptions f (ScatterPlot v) d where
  gscatterOptions f s@(ScatterPlot p) =
    case eq p of
      Just Refl -> ScatterPlot <$> f p
      Nothing   -> pure s
    where
      eq :: Typeable a => a -> Maybe (a :~: ScatterOptions v d)
      eq _ = eqT

instance (Functor f, HasScatterOptions f p a) => HasScatterOptions f (Plot p) a where
  gscatterOptions = rawPlot . gscatterOptions

instance (Applicative f, Typeable v, Typeable a)
    => HasScatterOptions f (DynamicPlot v) a where
  gscatterOptions = dynamicPlot . rawPlot

instance (Applicative f, Typeable (BaseSpace c), Typeable a)
    => HasScatterOptions f (Axis c) a where
  gscatterOptions = axisPlots . traverse . gscatterOptions


-- Pure scatter lenses -------------------------------------------------

-- | Lens onto a scatter plot of points.
scatterOptions :: (InSpace v Double a, HasScatterOptions f a (Point v Double))
               => LensLike' f a (ScatterOptions v (Point v Double))
scatterOptions = gscatterOptions

-- -- | Lens onto a transform of a scatter plot of points. This is a
-- --   specialised version of 'scatterTransform' for better type
-- --   inference.
-- _scatterTransform
--   :: (InSpace v a, PointLike v p, Functor f, HasScatterOptions f a (Point v))
--   => LensLike' f a (p -> Transformation v)
-- _scatterTransform = scatterTransform . lmapping unpointLike

-- -- | Lens onto a transform of a scatter plot of points. This is a
-- --   specialised version of 'scatterPosition' for better type inference.
-- _scatterPosition
--   :: (InSpace v a, PointLike v p, Functor f, HasScatterOptions f a (Point v))
--   => LensLike' f a (p -> p)
-- _scatterPosition = scatterPos . dimapping unpointLike pointLike

-- -- | Lens onto a style function of a scatter plot of points. This is a
-- --   specialised version of 'scatterStyle' for better type inference.
-- _scatterStyle
--   :: (InSpace v a, PointLike v p, Functor f, HasScatterOptions f a (Point v))
--   => LensLike' f a (p -> Style v)
-- _scatterStyle = scatterStyle . lmapping unpointLike

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

-- $ scatter
-- Scatter plots display data as dots. There are several representations
-- for scatter plots for extra parameters. Scatter plots have the
-- following lenses:
--
-- * 'connectingLine' - line between points
-- *
--

-- | Add a 'ScatterPlot' to the 'AxisState' from a data set.
--
-- @
-- 'scatterPlot' :: [('Double', 'Double')] -> 'State' ('Plot' ('ScatterOptions' 'V2' ('P2' 'Double')) b) () -> 'State' ('Axis' 'V2') ()
-- 'scatterPlot' :: ['V2' 'Double']        -> 'State' ('Plot' ('ScatterOptions' 'V2' ('P2' 'Double')) b) () -> 'State' ('Axis' 'V2') ()
-- 'scatterPlot' :: ['P2' 'Double']        -> 'State' ('Plot' ('ScatterOptions' 'V2' ('P2' 'Double')) b) () -> 'State' ('Axis' 'V2') ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Scatter_scatterExample.svg#diagram=scatterExample&height=350>>
--
-- > import Plots
-- > mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- > mydata2 = mydata1 & each . _1 *~ 0.5
-- > mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- > scatterAxis :: Axis V2
-- > scatterAxis = r2Axis &~ do
-- >   scatterPlot mydata1 $ key "data 1"
-- >   scatterPlot mydata2 $ key "data 2"
-- >   scatterPlot mydata3 $ key "data 3"
--
-- > scatterExample = renderAxis scatterAxis
scatterPlot
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      MonadState (Axis c) m,
      Plotable (ScatterPlot v),
      F.Foldable f)
  => f p  -- ^ points to plot
  -> State (Plot (ScatterOptions v (Point v Double))) ()
          -- ^ changes to plot options
  -> m () -- ^ add plot to 'Axis'
scatterPlot xs = gscatterPlot (xs ^.. folded . unpointLike) id

-- | Version of 'scatterPlot' without any changes to the
--   'ScatterOptions'.
--
-- @
-- 'scatterPlot'' :: [('Double', 'Double')] -> 'State' ('Axis' b 'V2' 'Double') ()
-- 'scatterPlot'' :: ['V2' 'Double']        -> 'State' ('Axis' b 'V2' 'Double') ()
-- 'scatterPlot'' :: ['P2' 'Double']        -> 'State' ('Axis' b 'V2' 'Double') ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Scatter_scatterExample'.svg#diagram=scatterExample'&height=350>>
--
-- > import Plots
-- > mydata4 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- > mydata5 = mydata1 & each . _1 *~ 0.5
-- > mydata6 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- > scatterAxis' :: Axis B V2 Double
-- > scatterAxis' = r2Axis &~ do
-- >   scatterPlot' mydata4
-- >   scatterPlot' mydata5
-- >   scatterPlot' mydata6
--
-- > scatterExample' = renderAxis scatterAxis'
scatterPlot'
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      MonadState (Axis c) m,
      Plotable (ScatterPlot v),
      F.Foldable f)
  => f p  -- ^ points to plot
  -> m () -- ^ add plot to 'Axis'
scatterPlot' xs = scatterPlot xs (return ())

-- | Version of 'scatterPlot' that accepts a 'Fold' over the data.
scatterPlotOf
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      Plotable (ScatterPlot v),
      MonadState (Axis c) m)
  => Fold s p -- ^ fold over points
  -> s        -- ^ data to fold
  -> State (Plot (ScatterOptions v (Point v Double))) () -- ^ changes to plot options
  -> m () -- ^ add plot to 'Axis'
scatterPlotOf f s = scatterPlot (toListOf f s)

-- | Version of 'scatterPlot' that accepts a 'Fold' over the data
--   without any changes to the 'ScatterOptions'.
scatterPlotOf'
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      MonadState (Axis c) m,
      Plotable (ScatterPlot v))
  => Fold s p -- ^ fold over points
  -> s -- ^ data to fold
  -> m () -- ^ add plot to axis
scatterPlotOf' f s = scatterPlot' (toListOf f s)

------------------------------------------------------------------------
-- Bubble plot --
------------------------------------------------------------------------

-- | A bubble plot is a scatter plot using point together with a scalar.
type BubbleOptions v = ScatterOptions v (Double, Point v Double)

-- | Scatter plots with extra numeric parameter. By default the extra
--   parameter is the scale of the marker but this can be changed.
--
-- @
-- 'bubblePlot' :: [('Double', ('Double', 'Double'))] -> 'State' ('Plot' ('BubbleOptions' v) b) () -> 'State' ('Axis' b 'V2' 'Double') ()
-- 'bubblePlot' :: [('Double', 'V2' 'Double')]        -> 'State' ('Plot' ('BubbleOptions' v) b) () -> 'State' ('Axis' b 'V2' 'Double') ()
-- 'bubblePlot' :: [('Double', 'P2' 'Double')]        -> 'State' ('Plot' ('BubbleOptions' v) b) () -> 'State' ('Axis' b 'V2' 'Double') ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Scatter_bubbleExample.svg#diagram=bubbleExample&height=350>>
--
-- > import Plots
-- > myweights = [2, 1.3, 1.8, 0.7]
-- > mydata7 = zip myweights [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- > mydata8 = mydata7 & each._2._2 *~ 0.5 & each._1 *~ 0.5
-- > mydata9 = [(1, V2 1.2 2.7), (3, V2 2 5.1), (0.9, V2 3.2 2.6), (2, V2 3.5 5)]
--
-- > bubbleAxis :: Axis B V2 Double
-- > bubbleAxis = r2Axis &~ do
-- >   bubblePlot mydata7 $ key "data 7"
-- >   bubblePlot mydata8 $ key "data 8"
-- >   bubblePlot mydata9 $ key "data 9"
--
-- > bubbleExample = renderAxis bubbleAxis
bubblePlot
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      MonadState (Axis c) m,
      Plotable (ScatterPlot v),
      F.Foldable f)
  => f (Double, p) -- ^ fold over points with a size
  -> State (Plot (BubbleOptions v)) () -- ^ changes to the options
  -> m () -- ^ add plot to 'Axis'
bubblePlot xs s =
  gscatterPlot (xs ^.. folded . mapping unpointLike) snd $ do
    bubbleTransform .= scaling
    s

-- | Simple version of 'bubblePlot' without any changes to the 'Plot'.
--
-- @
-- 'bubblePlot'' :: [('Double', ('Double', 'Double'))] -> 'State' ('Axis' b 'V2' 'Double') ()
-- 'bubblePlot'' :: [('Double', 'V2' 'Double')]        -> 'State' ('Axis' b 'V2' 'Double') ()
-- @
--
bubblePlot'
  :: (v ~ BaseSpace c,
      PointLike v Double p,
      MonadState (Axis c) m,
      Plotable (ScatterPlot v),
      F.Foldable f)
  => f (Double, p) -- ^ fold over points with a size
  -> m () -- ^ add plot to 'Axis'
bubblePlot' xs = bubblePlot xs (return ())

-- | Version of 'bubblePlot' using a 'Fold' over the data.
bubblePlotOf
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      Plotable (ScatterPlot v),
      MonadState (Axis c) m)
  => Fold s (Double,p) -- ^ fold over the data
  -> s            -- ^ data
  -> State (Plot (BubbleOptions v)) ()
                  -- ^ changes to the options
  -> m ()         -- ^ add plot to 'Axis'
bubblePlotOf f s = bubblePlot (toListOf f s)

-- | Version of 'bubblePlot' using a 'Fold' over the data without any
--   changes to the 'BubbleOptions'.
bubblePlotOf'
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      MonadState (Axis c) m,
      Plotable (ScatterPlot v))
  => Fold s (Double,p) -- ^ fold over the data
  -> s            -- ^ data
  -> State (Plot (BubbleOptions v)) ()
                  -- ^ changes to the options
  -> m ()         -- ^ add plot to 'Axis'
bubblePlotOf' f s = bubblePlot (toListOf f s)

-- Bubble scatter lenses -----------------------------------------------

-- | LensLike onto into a 'ScatterOptions' made up of a scaler @n@, and
--   a point, @'Point' v@
--
-- @
-- 'bubbleOptions' :: 'Lens'' ('Plot' ('BubbleOptions' v) v) ('BubbleOptions' v)
-- @
bubbleOptions :: (InSpace v Double a, HasScatterOptions f a (Double, Point v Double))
              => LensLike' f a (BubbleOptions v)
bubbleOptions = gscatterOptions

-- | Setter over the transform function for a 'bubblePlot'. Default is 'scale'.
--
-- @
-- 'bubbleOptions' :: 'Setter'' ('Plot' ('BubbleOptions' v) v) (n -> 'Transformation' v)
-- @
--
--   Note that this is the less general version of @'bubblePlot' .
--   'scatterTransform'@, which would give a 'LensLike' onto @(n,
--   'Point' v) -> 'Transformation' v@.
--
bubbleTransform
  :: (InSpace v Double a, HasScatterOptions f a (Double, Point v Double), Settable f)
  => LensLike' f a (Double -> Transformation v Double)
bubbleTransform = bubbleOptions . scatterTransform . sets nOnly
  where nOnly f g (n,p) = f (\n' -> g (n', p)) n

-- | Setter over the style function for a 'bubblePlot'. Default is 'mempty'.
--
-- @
-- 'bubbleStyle' :: 'Setter'' ('Plot' ('BubbleOptions' v) v) (n -> 'Style' v)
-- @
--
--   Note that this is the less general version of @'bubblePlot' .
--   'scatterTransform'@, which would give a 'LensLike' onto @(n,
--   'Point' v) -> 'Style' v@.
--
bubbleStyle :: (InSpace v Double a, Settable f, HasScatterOptions f a (Double, Point v Double))
             => LensLike' f a (Double -> Style v Double)
bubbleStyle = bubbleOptions . scatterStyle . sets nOnly
  where nOnly f g (n,p) = f (\n' -> g (n', p)) n

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

-- | A general scatter plot allow using any data type @d@ to determine
--   the 'scatterTransform' and 'scatterStyle'.
gscatterPlot
  :: (BaseSpace c ~ v,
      PointLike v Double p,
      MonadState (Axis c) m,
      Typeable d,
      Plotable (ScatterPlot v),
      F.Foldable f)
  => f d -- ^ data
  -> (d -> p) -- ^ extract point from data
  -> State (Plot (ScatterOptions v d)) ()
              -- ^ options for plot
  -> m ()     -- ^ add plot to 'Axis'
gscatterPlot xs pf s = addPlot $ over rawPlot ScatterPlot p1
  where
    p1 = execState s p0
    p0 = mkPlot $ mkScatterOptions xs (view unpointLike . pf)

-- | Helper to traverse over a general scatter plot where the type of d
--   is not infered.
gscatterOptionsFor
  :: (InSpace v Double a, HasScatterOptions f a d)
  => proxy d -> LensLike' f a (ScatterOptions v d)
gscatterOptionsFor _ = gscatterOptions

