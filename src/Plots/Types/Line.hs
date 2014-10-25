{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}

module Plots.Types.Line
  ( LinePlot
    -- * Constucting Line Plots
  , mkLinePlotFromVerticies
  , mkLinePlotFromPath
  , mkMultiLinePlotFromVerticies

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
-- import Diagrams.ThreeD.Types
import Diagrams.BoundingBox

import Diagrams.Coordinates.Isomorphic

import Plots.Themes
import Plots.Types

-- | Plottable data describing how to draw a line plot.
data LinePlot b v n = LinePlot
  { _linePlotPath    :: Path v n
  , _linePlotGeneric :: GenericPlot b v n
  } deriving Typeable

makeLensesWith (lensRules & generateSignatures .~ False) ''LinePlot

type instance V (LinePlot b v n) = v
type instance N (LinePlot b v n) = n
type instance B (LinePlot b v n) = b

instance HasGenericPlot (LinePlot b v n) where
  genericPlot = linePlotGeneric


-- | Empty path.
instance (HasLinearMap v, TypeableFloat n, Renderable (Path V2 n) b) => Default (LinePlot b v n) where
  def = LinePlot mempty def

-- instance Renderable (Path R2) b => Default (LinePlot b R3) where
--   def = LinePlot mempty def


instance (OrderedField n, TypeableFloat n, Typeable b, Typeable v, Renderable (Path V2 n) b,
          HasLinearMap v, Metric v)
       => Plotable (LinePlot b v n) where
  plot _ tv l t2 lp = lp ^.linePlotPath
                   & transform tv
                   & lmap l
                   & transform t2
                   & stroke
                   & applyStyle (lp ^. themeLineStyle)

  defLegendPic a = (p2 (-10,0) ~~ p2 (10,0)) # applyStyle (a ^. themeLineStyle)


-- | Prism onto the @Plot@ wrapper. Wrap a line plot in a plot by
--
-- @@
-- review _LinePlot myLinePlot
-- @@
_LinePlot :: Plotable (LinePlot b v n) => Prism' (Plot b v n) (LinePlot b v n)
_LinePlot = _Plot

-- | Lens onto the path the line plot will draw. In most cases it's best to use
--   mkLinePlot.
linePlotPath :: Lens' (LinePlot b v n) (Path v n)

linePlotGeneric :: Lens' (LinePlot b v n) (GenericPlot b v n)

-- LinePlotable

mkLinePlotFromVerticies :: (PointLike v n a, OrderedField n, Metric v, Foldable f, Default (LinePlot b v n))
    => f a -> LinePlot b v n
mkLinePlotFromVerticies a =
  def & linePlotPath    .~ fromVertices points
      & plotBoundingBox .~ fromPoints points
    where
      points = a ^.. folded . re pointLike

mkMultiLinePlotFromVerticies :: (PointLike v n a, TypeableFloat n, HasLinearMap v, Metric v, Foldable f, Foldable g, Renderable (Path V2 n) b)
    => f (g a) -> LinePlot b v n
mkMultiLinePlotFromVerticies a = mkLinePlotFromPath p
  where p         = foldMap mkPath a
        mkPath as = fromVertices $ as ^.. folded . re pointLike

mkLinePlotFromPath :: (HasBasis v, Metric v, HasLinearMap v, OrderedField n, Default (LinePlot b v n)) => Path v n -> LinePlot b v n
mkLinePlotFromPath p =
  def & linePlotPath    .~ p
      & plotBoundingBox .~ boundingBox p

-- -- | Class of things that can be easily made into line plots.
-- class LinePlotable a b v where
--   mkLinePlot :: a -> LinePlot b v
-- 
-- instance LinePlotable (LinePlot b v) b v where
--   mkLinePlot = id
-- 
-- instance Renderable (Path R2) b => LinePlotable (Path R2) b R2 where
--   mkLinePlot path = def & linePlotPath    .~ path
--                         & plotBoundingBox .~ boundingBox path
-- 
-- instance Renderable (Path R2) b => LinePlotable (Path R3) b R3 where
--   mkLinePlot path = def & linePlotPath    .~ path
--                         & plotBoundingBox .~ boundingBox path
-- 
-- instance (Renderable (Path R2) b, Foldable t)
--   => LinePlotable (t (Double, Double, Double)) b R3 where
--   mkLinePlot = mkLinePlot . (fromVertices :: [P3] -> Path R3) . map p3 . F.toList
-- 
-- instance (Renderable (Path R2) b, Foldable t)
--   => LinePlotable (t P3) b R3 where
--   mkLinePlot = mkLinePlot . (fromVertices :: [P3] -> Path R3) . F.toList
-- 
-- instance (Renderable (Path R2) b, Foldable t)
--   => LinePlotable (t (Double, Double)) b R2 where
--   mkLinePlot = mkLinePlot . (fromVertices :: [P2] -> Path R2) . map p2 . F.toList
-- 
-- instance (Renderable (Path R2) b, Foldable t)
--   => LinePlotable (t P2) b R2 where
--   mkLinePlot = mkLinePlot . (fromVertices :: [P2] -> Path R2) . F.toList

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


-- -- MultiLinePlotable
-- 
-- -- | Class of things that can be easily made into multi line plots. Essentially 
-- --   the same as LinePlotable; only needed to prevent overlapping instances.
-- class MultiLinePlotable a b v where
--   mkMultiLinePlot :: a -> LinePlot b v
-- 
-- instance MultiLinePlotable (LinePlot b v) b v where
--   mkMultiLinePlot = id
-- 
-- instance Renderable (Path R2) b => MultiLinePlotable (Path R2) b R2 where
--   mkMultiLinePlot path = def & linePlotPath .~ path
-- 
-- instance (Renderable (Path R2) b, Foldable t, Foldable s)
--   => MultiLinePlotable (s (t (Double, Double))) b R2 where
--   mkMultiLinePlot = mkLinePlot . F.foldMap ((fromVertices :: [P2] -> Path R2) . map p2 . F.toList)
-- 
-- instance (Renderable (Path R2) b, Foldable t, Foldable s)
--   => MultiLinePlotable (s (t P2)) b R2 where
--   mkMultiLinePlot = mkLinePlot . F.foldMap ((fromVertices :: [P2] -> Path R2) . F.toList)
-- 
-- -- addMultiLinePlotable :: (Typeable b, Renderable (Path R2) b, MultiLinePlotable a b R2)
-- --                      => a -> Axis b R2 -> Axis b R2
-- -- addMultiLinePlotable t a = a & axisPlots <>~ [review _LinePlot $ mkMultiLinePlot t]
