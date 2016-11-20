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
data ScatterPlot v n where
  ScatterPlot :: Typeable a => ScatterOptions v n a -> ScatterPlot v n
  deriving Typeable

type instance V (ScatterPlot v n) = v
type instance N (ScatterPlot v n) = n

-- | A general data type for scatter plots. Allows storing different
--   types of data as well as allowing transforms depending on the data.
data ScatterOptions v n a = ScatterOptions
  { oData :: [a]
  , oPos  :: a -> Point v n
  , oTr   :: a -> Transformation v n
  , oSty  :: a -> Style v n
  , oLine :: Bool
  } deriving Typeable

type instance V (ScatterOptions v n a) = v
type instance N (ScatterOptions v n a) = n

instance (Metric v, OrderedField n) => Enveloped (ScatterPlot v n) where
  getEnvelope (ScatterPlot (ScatterOptions {..})) = getEnvelope (map oPos oData)

instance (TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (ScatterPlot V2 n) b where
  renderPlotable s sty (ScatterPlot (ScatterOptions {..})) =
    markers <> line
    where
      markers = foldMap mk oData # applyMarkerStyle sty
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
  :: (PointLike v n p, F.Foldable f, Fractional n)
  => f a
  -> (a -> p)
  -> ScatterOptions v n a
mkScatterOptions xs pf = ScatterOptions
  { oData = F.toList xs
  , oPos  = view unpointLike . pf
  , oTr   = mempty
  , oSty  = const (_Wrapped ## mempty)
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

instance HasConnectingLine f (ScatterOptions v n a) where
  connectingLine = lens oLine (\o b -> o {oLine = b})

instance HasConnectingLine f (ScatterPlot v n) where
  connectingLine f (ScatterPlot o@(ScatterOptions {..}))
    = f oLine <&> \b -> ScatterPlot o {oLine = b}

instance HasConnectingLine f p => HasConnectingLine f (Plot p b) where
  connectingLine = rawPlot . connectingLine

instance (Applicative f, Typeable b, Typeable v, Typeable n)
    => HasConnectingLine f (DynamicPlot b v n) where
  connectingLine = (dynamicPlot :: Traversal' (DynamicPlot b v n) (Plot (ScatterPlot v n) b))
                 . connectingLine

instance (Applicative f, Typeable v, Typeable n)
    => HasConnectingLine f (StyledPlot b v n) where
  connectingLine = (styledPlot :: Traversal' (StyledPlot b v n) (ScatterPlot v n))
                 . connectingLine

instance (Settable f, Typeable (BaseSpace c), Typeable n)
    => HasConnectingLine f (Axis b c n) where
  connectingLine = finalPlots . connectingLine

-- Options -------------------------------------------------------------

class HasScatterOptions f a d where
  -- | Lens onto the 'ScatterOptions' for a general scatter plot.
  gscatterOptions :: LensLike' f a (ScatterOptions (V a) (N a) d)

  -- | Apply a transform to the markers using the associated data.
  scatterTransform :: Functor f => LensLike' f a (d -> Transformation (V a) (N a))
  scatterTransform = gscatterOptions . lens oTr (\o tr -> o {oTr = tr})

  -- | Apply a style to the markers using the associated data.
  scatterStyle :: Functor f => LensLike' f a (d -> Style (V a) (N a))
  scatterStyle = gscatterOptions . lens oSty (\o sty -> o {oSty = sty})

  -- | Change the position of the markers depending on the data.
  scatterPosition :: Functor f => LensLike' f a (d -> Point (V a) (N a))
  scatterPosition = gscatterOptions . lens oPos (\o pos -> o {oPos = pos})

instance d ~ d' => HasScatterOptions f (ScatterOptions v n d) d' where
  gscatterOptions = id

instance (Applicative f, Typeable v, Typeable n, Typeable d)
    => HasScatterOptions f (ScatterPlot v n) d where
  gscatterOptions f s@(ScatterPlot p) =
    case eq p of
      Just Refl -> ScatterPlot <$> f p
      Nothing   -> pure s
    where
      eq :: Typeable a => a -> Maybe (a :~: ScatterOptions v n d)
      eq _ = eqT

instance (Functor f, HasScatterOptions f p a) => HasScatterOptions f (Plot p b) a where
  gscatterOptions = rawPlot . gscatterOptions

instance (Applicative f, Typeable b, Typeable v, Typeable n, Typeable a)
    => HasScatterOptions f (DynamicPlot b v n) a where
  gscatterOptions = dynamicPlot . rawPlot

instance (Applicative f, Typeable b, Typeable (BaseSpace c), Typeable n, Typeable a)
    => HasScatterOptions f (Axis b c n) a where
  gscatterOptions = axisPlots . traverse . gscatterOptions


-- Pure scatter lenses -------------------------------------------------

-- | Lens onto a scatter plot of points.
scatterOptions :: (InSpace v n a, HasScatterOptions f a (Point v n))
               => LensLike' f a (ScatterOptions v n (Point v n))
scatterOptions = gscatterOptions

-- -- | Lens onto a transform of a scatter plot of points. This is a
-- --   specialised version of 'scatterTransform' for better type
-- --   inference.
-- _scatterTransform
--   :: (InSpace v n a, PointLike v n p, Functor f, HasScatterOptions f a (Point v n))
--   => LensLike' f a (p -> Transformation v n)
-- _scatterTransform = scatterTransform . lmapping unpointLike

-- -- | Lens onto a transform of a scatter plot of points. This is a
-- --   specialised version of 'scatterPosition' for better type inference.
-- _scatterPosition
--   :: (InSpace v n a, PointLike v n p, Functor f, HasScatterOptions f a (Point v n))
--   => LensLike' f a (p -> p)
-- _scatterPosition = scatterPos . dimapping unpointLike pointLike

-- -- | Lens onto a style function of a scatter plot of points. This is a
-- --   specialised version of 'scatterStyle' for better type inference.
-- _scatterStyle
--   :: (InSpace v n a, PointLike v n p, Functor f, HasScatterOptions f a (Point v n))
--   => LensLike' f a (p -> Style v n)
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
--   myaxis = r2Axis ~&
--     scatterPlot data1
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
-- > scatterAxis :: Axis B V2 Double
-- > scatterAxis = r2Axis &~ do
-- >   scatterPlot mydata1 $ key "data 1"
-- >   scatterPlot mydata2 $ key "data 2"
-- >   scatterPlot mydata3 $ key "data 3"
--
-- > scatterExample = renderAxis scatterAxis
scatterPlot
  :: (BaseSpace c ~ v,
      PointLike v n p,
      Typeable n,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p  -- ^ points to plot
  -> State (Plot (ScatterOptions v n (Point v n)) b) ()
          -- ^ changes to plot options
  -> m () -- ^ add plot to 'Axis'
scatterPlot xs = gscatterPlot (xs ^.. folded . unpointLike) id

-- | Version of 'scatterPlot' without any changes to the
--   'ScatterOptions'.
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
      PointLike v n p,
      Typeable n,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p  -- ^ points to plot
  -> m () -- ^ add plot to 'Axis'
scatterPlot' xs = scatterPlot xs (return ())

-- | Version of 'scatterPlot' that accepts a 'Fold' over the data.
scatterPlotOf
  :: (BaseSpace c ~ v,
      PointLike v n p,
      Typeable n,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -- ^ fold over points
  -> s        -- ^ data to fold
  -> State (Plot (ScatterOptions v n (Point v n)) b) () -- ^ changes to plot options
  -> m () -- ^ add plot to 'Axis'
scatterPlotOf f s = scatterPlot (toListOf f s)

-- | Version of 'scatterPlot' that accepts a 'Fold' over the data
--   without any changes to the 'ScatterOptions'.
scatterPlotOf'
  :: (BaseSpace c ~ v,
      PointLike v n p,
      Typeable n,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -- ^ fold over points
  -> s -- ^ data to fold
  -> m () -- ^ add plot to axis
scatterPlotOf' f s = scatterPlot' (toListOf f s)

------------------------------------------------------------------------
-- Bubble plot --
------------------------------------------------------------------------

-- | A bubble plot is a scatter plot using point together with a scalar.
type BubbleOptions v n = ScatterOptions v n (n, Point v n)

-- | Scatter plots with extra numeric parameter. By default the extra
--   parameter is the scale of the marker but this can be changed.
bubblePlot
  :: (BaseSpace c ~ v,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      Typeable n,
      F.Foldable f)
  => f (n, p) -- ^ fold over points with a size
  -> State (Plot (BubbleOptions v n) b) () -- ^ changes to the options
  -> m () -- ^ add plot to 'Axis'
bubblePlot xs s =
  gscatterPlot (xs ^.. folded . mapping unpointLike) snd $ do
    bubbleTransform .= scaling
    s

-- | Simple version of 'bubblePlot' without any changes to the 'Plot'.
bubblePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      Typeable n,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f (n, p) -- ^ fold over points with a size
  -> m () -- ^ add plot to 'Axis'
bubblePlot' xs = bubblePlot xs (return ())

-- | Version of 'bubblePlot' using a 'Fold' over the data.
bubblePlotOf
  :: (BaseSpace c ~ v,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      Typeable n)
  => Fold s (n,p) -- ^ fold over the data
  -> s            -- ^ data
  -> State (Plot (BubbleOptions v n) b) ()
                  -- ^ changes to the options
  -> m ()         -- ^ add plot to 'Axis'
bubblePlotOf f s = bubblePlot (toListOf f s)

-- | Version of 'bubblePlot' using a 'Fold' over the data without any
--   changes to the 'BubbleOptions'.
bubblePlotOf'
  :: (BaseSpace c ~ v,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      Typeable n)
  => Fold s (n,p) -- ^ fold over the data
  -> s            -- ^ data
  -> State (Plot (BubbleOptions v n) b) ()
                  -- ^ changes to the options
  -> m ()         -- ^ add plot to 'Axis'
bubblePlotOf' f s = bubblePlot (toListOf f s)

-- Bubble scatter lenses -----------------------------------------------

-- | LensLike onto into a 'ScatterOptions' made up of a scaler @n@, and
--   a point, @'Point' v n@
--
-- @
-- 'bubbleOptions' :: 'Lens'' ('Plot' ('BubbleOptions' v n) v) ('BubbleOptions' v n)
-- @
bubbleOptions :: (InSpace v n a, HasScatterOptions f a (n, Point v n))
              => LensLike' f a (BubbleOptions v n)
bubbleOptions = gscatterOptions

-- | Setter over the transform function for a 'bubblePlot'. Default is 'scale'.
--
-- @
-- 'bubbleOptions' :: 'Setter'' ('Plot' ('BubbleOptions' v n) v) (n -> 'Transformation' v n)
-- @
--
--   Note that this is the less general version of @'bubblePlot' .
--   'scatterTransform'@, which would give a 'LensLike' onto @(n,
--   'Point' v n) -> 'Transformation' v n@.
--
bubbleTransform
  :: (InSpace v n a, HasScatterOptions f a (n, Point v n), Settable f)
  => LensLike' f a (n -> Transformation v n)
bubbleTransform = bubbleOptions . scatterTransform . sets nOnly
  where nOnly f g (n,p) = f (\n' -> g (n', p)) n

-- | Setter over the style function for a 'bubblePlot'. Default is 'mempty'.
--
-- @
-- 'bubbleStyle' :: 'Setter'' ('Plot' ('BubbleOptions' v n) v) (n -> 'Style' v n)
-- @
--
--   Note that this is the less general version of @'bubblePlot' .
--   'scatterTransform'@, which would give a 'LensLike' onto @(n,
--   'Point' v n) -> 'Style' v n@.
--
bubbleStyle :: (InSpace v n a, Settable f, HasScatterOptions f a (n, Point v n))
             => LensLike' f a (n -> Style v n)
bubbleStyle = bubbleOptions . scatterStyle . sets nOnly
  where nOnly f g (n,p) = f (\n' -> g (n', p)) n

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

-- | A general scatter plot allow using any data type @d@ to determine
--   the 'scatterTransform' and 'scatterStyle'.
gscatterPlot
  :: (BaseSpace c ~ v,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      Typeable d,
      F.Foldable f)
  => f d -- ^ data
  -> (d -> p) -- ^ extract point from data
  -> State (Plot (ScatterOptions v n d) b) ()
              -- ^ options for plot
  -> m ()     -- ^ add plot to 'Axis'
gscatterPlot xs pf s = addPlot $ over rawPlot ScatterPlot p1
  where
    p1 = execState s p0
    p0 = mkPlot $ mkScatterOptions xs (view unpointLike . pf)

-- | Helper to traverse over a general scatter plot where the type of d
--   is not infered.
gscatterOptionsFor
  :: (InSpace v n a, HasScatterOptions f a d)
  => proxy d -> LensLike' f a (ScatterOptions v n d)
gscatterOptionsFor _ = gscatterOptions

