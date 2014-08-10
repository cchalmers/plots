{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Types.Line
  ( LinePlot
    -- * Constucting Line Plots
  , LinePlotable (..)
  , MultiLinePlotable (..)

    -- * Prism
  , _LinePlot

    -- * Lenses
  , linePlotPath
  ) where

import Control.Lens     hiding (transform, ( # ), lmap)
import Data.Default
import Data.Foldable    as F
import Data.Typeable
import Diagrams.Prelude
import Diagrams.LinearMap
import Diagrams.ThreeD.Types
import Diagrams.BoundingBox

import Plots.Themes
import Plots.Types

-- | Plottable data describing how to draw a line plot.
data LinePlot b v = LinePlot
  { _linePlotPath    :: Path v
  , _linePlotGeneric :: GenericPlot b v
  } deriving Typeable

makeLensesWith (lensRules & generateSignatures .~ False) ''LinePlot

type instance V (LinePlot b v) = R2

instance HasGenericPlot (LinePlot b v) b v where
  genericPlot = linePlotGeneric

instance HasStyle (LinePlot b v) where
  applyStyle sty = over (genericPlot . themeLineStyle)   (applyStyle sty)
                 . over (genericPlot . themeMarkerStyle) (applyStyle sty)
                 . over (genericPlot . themeFillStyle)   (applyStyle sty)


-- | Empty path.
instance Renderable (Path R2) b => Default (LinePlot b R2) where
  def = LinePlot mempty def

instance Renderable (Path R2) b => Default (LinePlot b R3) where
  def = LinePlot mempty def


instance (Typeable b, Typeable v, Scalar v ~ Double, Renderable (Path R2) b,
          HasLinearMap v, InnerSpace v)
       => Plotable (LinePlot b v) b v where
  plot _ l t lp = lp ^.linePlotPath
                   & lmap l
                   & transform t
                   & stroke
                   & applyStyle (lp^.plotLineStyle)

-- | Prism onto the @Plot@ wrapper. Wrap a line plot in a plot by
--
-- @@
-- review _LinePlot myLinePlot
-- @@
_LinePlot :: Plotable (LinePlot b v) b v => Prism' (Plot b v) (LinePlot b v)
_LinePlot = _Plot

-- | Lens onto the path the line plot will draw. In most cases it's best to use
--   mkLinePlot.
linePlotPath :: Lens' (LinePlot b v) (Path v)

linePlotGeneric :: Lens' (LinePlot b v) (GenericPlot b v)

-- LinePlotable

-- | Class of things that can be easily made into line plots.
class LinePlotable a b v where
  mkLinePlot :: a -> LinePlot b v

instance LinePlotable (LinePlot b v) b v where
  mkLinePlot = id

instance Renderable (Path R2) b => LinePlotable (Path R2) b R2 where
  mkLinePlot path = def & linePlotPath    .~ path
                        & plotBoundingBox .~ boundingBox path

instance Renderable (Path R2) b => LinePlotable (Path R3) b R3 where
  mkLinePlot path = def & linePlotPath    .~ path
                        & plotBoundingBox .~ boundingBox path

instance (Renderable (Path R2) b, Foldable t)
  => LinePlotable (t (Double, Double, Double)) b R3 where
  mkLinePlot = mkLinePlot . (fromVertices :: [P3] -> Path R3) . map p3 . F.toList

instance (Renderable (Path R2) b, Foldable t)
  => LinePlotable (t P3) b R3 where
  mkLinePlot = mkLinePlot . (fromVertices :: [P3] -> Path R3) . F.toList

instance (Renderable (Path R2) b, Foldable t)
  => LinePlotable (t (Double, Double)) b R2 where
  mkLinePlot = mkLinePlot . (fromVertices :: [P2] -> Path R2) . map p2 . F.toList

instance (Renderable (Path R2) b, Foldable t)
  => LinePlotable (t P2) b R2 where
  mkLinePlot = mkLinePlot . (fromVertices :: [P2] -> Path R2) . F.toList

-- | Convient function for adding a line plot to an axis:
--
-- @@
-- myAxis = defaultAxis
--            # addLinePlotable [(1,1), (2,3), (1.5,4)]
-- @@
-- addLinePlotable :: (Typeable b, Renderable (Path R2) b, LinePlotable a b R2)
--                 => a -> Axis b R2 -> Axis b R2
-- addLinePlotable t a = a & axisPlots <>~ [review _LinePlot $ mkLinePlot t]

-- | Same as addLinePlotable but takes a function allowing adjustments, i.e. adding a legend:
--
-- @@
-- myAxis = defaultAxis
--            # addLinePlotable (addLegendEntry "thick line" . lwG 3) [(1,1), (2,3), (1.5,4)]
-- @@
-- addLinePlotable' :: (Typeable b, Renderable (Path R2) b, LinePlotable a b R2)
--                  => (LinePlot b R2 -> LinePlot b R2) -> a -> Axis b R2 -> Axis b R2
-- addLinePlotable' f t a = a & axisPlots <>~ [review _LinePlot . f $ mkLinePlot t]


-- MultiLinePlotable

-- | Class of things that can be easily made into multi line plots. Essentially 
--   the same as LinePlotable; only needed to prevent overlapping instances.
class MultiLinePlotable a b v where
  mkMultiLinePlot :: a -> LinePlot b v

instance MultiLinePlotable (LinePlot b v) b v where
  mkMultiLinePlot = id

instance Renderable (Path R2) b => MultiLinePlotable (Path R2) b R2 where
  mkMultiLinePlot path = def & linePlotPath .~ path

instance (Renderable (Path R2) b, Foldable t, Foldable s)
  => MultiLinePlotable (s (t (Double, Double))) b R2 where
  mkMultiLinePlot = mkLinePlot . F.foldMap ((fromVertices :: [P2] -> Path R2) . map p2 . F.toList)

instance (Renderable (Path R2) b, Foldable t, Foldable s)
  => MultiLinePlotable (s (t P2)) b R2 where
  mkMultiLinePlot = mkLinePlot . F.foldMap ((fromVertices :: [P2] -> Path R2) . F.toList)

-- addMultiLinePlotable :: (Typeable b, Renderable (Path R2) b, MultiLinePlotable a b R2)
--                      => a -> Axis b R2 -> Axis b R2
-- addMultiLinePlotable t a = a & axisPlots <>~ [review _LinePlot $ mkMultiLinePlot t]
