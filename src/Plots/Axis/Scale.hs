{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Scale
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Determine how to scale an axis.
--
----------------------------------------------------------------------------

module Plots.Axis.Scale
  ( -- * Axis scale
    AxisScaling
  , ScaleMode (..)
  , UniformScaleStrategy (..)
  , Extending (..)
  , noExtend
  , HasAxisScaling (..)

   -- ** Log scales
  , LogScale (..)
  , logNumber
  , logPoint
  -- , logDeform

    -- * Low level calculations
    -- | These functions are used by "Plots.Axis.Render".
  , calculateBounds
  , calculateScaling

  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Bool
import           Data.Default
import           Data.Distributive
import           Data.Maybe
-- import qualified Data.Foldable as F

import           Diagrams
import           Linear

------------------------------------------------------------------------
-- Axis scale
------------------------------------------------------------------------

-- | How the axis should be scaled when not all dimensions are set.
data ScaleMode
  = AutoScale
  | NoScale
  | Stretch
  | UniformScale UniformScaleStrategy
  deriving (Show, Read)

-- | ?
data UniformScaleStrategy
  = AutoUniformScale
  | UnitOnly
  | ChangeVerticalLimits
  | ChangeHorizontalLimits
  deriving (Show, Read)

-- | Data type used that concerns everything to do with the size or
--   scale of the axis.
data AxisScaling = Scaling
  { asRatio          :: Maybe Double
  , asMode           :: ScaleMode
  , asEnlarge        :: Extending
  , asBoundMin       :: Maybe Double
  , asBoundMax       :: Maybe Double
  , asSize           :: Maybe Double
  , asLogScale       :: LogScale

  -- backup bound in case there's no inferred bounds to go by
  , asBackupBoundMax :: Double
  , asBackupBoundMin :: Double
  }

type instance N AxisScaling = Double

instance Default AxisScaling where
  def = Scaling
    { asRatio          = Nothing
    , asMode           = AutoScale
    , asEnlarge        = RelativeExtend 0.1
    , asBoundMin       = Nothing
    , asBoundMax       = Nothing
    , asLogScale       = def
    , asSize           = Just 400
    , asBackupBoundMax = 5
    , asBackupBoundMin = -5
    }

-- | How much to extend the bounds beyond any inferred bounds.
data Extending
  = AbsoluteExtend !Double
  | RelativeExtend !Double
  deriving (Show, Ord, Eq)

-- | Do not extend the axis beyond the inferred bounds.
noExtend :: Extending
noExtend = AbsoluteExtend 0

-- | Class of things that have an 'AxisScaling'.
class HasAxisScaling f a where
  -- | The way to scale in one direction.
  axisScaling :: LensLike' f a AxisScaling

  -- | The ratio relative to other axis. If no ratios are set, the ratio
  --   is not enforced. If at least one is set, 'Nothing' ratios are
  --   @1@.
  scaleAspectRatio :: Functor f => LensLike' f a (Maybe Double)
  scaleAspectRatio = axisScaling . lens asRatio (\as r -> as {asRatio = r})

  -- | The mode to determine how to scale the bounds in a direction.
  --   Choose between 'AutoScale', 'NoScale', 'Stretch' or
  --   'UniformScale'.
  --
  --   'Default' is 'AutoScale'.
  scaleMode :: Functor f => LensLike' f a ScaleMode
  scaleMode = axisScaling . lens asMode (\as r -> as {asMode = r})

  -- | Whether the axis uses 'LogAxis' or 'LinearAxis'.
  --
  --   'Default' is 'LinearAxis'.
  logScale :: Functor f => LensLike' f a LogScale
  logScale = axisScaling . lens asLogScale (\as r -> as {asLogScale = r})

  -- | How much to extend the bounds over infered bounds. This is
  --   ignored if a 'boundMax' or 'boundMin' is set.
  axisExtend :: Functor f => LensLike' f a Extending
  axisExtend = axisScaling . lens asEnlarge (\as r -> as {asEnlarge = r})

  -- | The maximum bound the axis. There are helper functions for
  --   setting a minimum bound for a specific axis.
  --
  -- @
  -- 'Plots.Axis.xMin' :: 'Lens'' ('Axis' b 'V2' 'Double') ('Maybe' 'Double')
  -- 'Plots.Axis.yMin' :: 'Lens'' ('Axis' b 'V2' 'Double') ('Maybe' 'Double')
  -- @
  --
  --   Default is 'Nothing'.
  boundMin :: Functor f => LensLike' f a (Maybe Double)
  boundMin = axisScaling . lens asBoundMin (\as b -> as {asBoundMin = b})

  -- | The maximum bound the axis. There are helper functions for
  --   setting a maximum bound specific axis.
  --
  -- @
  -- 'Plots.Axis.xMax' :: 'Lens'' ('Axis' b 'V2' 'Double') ('Maybe' 'Double')
  -- 'Plots.Axis.yMax' :: 'Lens'' ('Axis' b 'V2' 'Double') ('Maybe' 'Double')
  -- 'Plots.Axis.rMax' :: 'Lens'' ('Axis' b 'Polar 'Double') ('Maybe' 'Double')
  -- @
  --
  --   Default is 'Nothing'.
  boundMax :: Functor f => LensLike' f a (Maybe Double)
  boundMax = axisScaling . lens asBoundMax (\as b -> as {asBoundMax = b})

  -- | The size of the rendered axis. Default is @'Just' 400@.
  renderSize :: Functor f => LensLike' f a (Maybe Double)
  renderSize = axisScaling . lens asSize (\as s -> as {asSize = s})

  -- -- backup bound in case there's no inferred bounds to go by
  -- asBackupBoundMax :: n
  -- asBackupBoundMax :: n

asSizeSpec :: HasLinearMap v => Lens' (v AxisScaling) (SizeSpec v Double)
asSizeSpec = column renderSize . iso mkSizeSpec getSpec

instance HasAxisScaling f AxisScaling where
  axisScaling = id

-- calculating bounds --------------------------------------------------

-- | Calculating the bounds for an axis.
calculateBounds
  :: AxisScaling -- ^ Scaling to use for this axis
  -> Maybe (Double, Double)  -- ^ Inferred bounds (from any plots)
  -> (Double, Double)        -- ^ Lower and upper bounds to use for this axis
calculateBounds Scaling {..} mInferred = (l', u') where
  -- bounds are only enlarged when min/max bound wasn't set
  l' = l & whenever (isNothing asBoundMin) (subtract x)
         & whenever (asLogScale == LogAxis) (max 1e-6)
  u' = u & whenever (isNothing asBoundMax) (+ x)

  -- amount to enlarge axis by
  x = case asEnlarge of
    AbsoluteExtend a -> a
    RelativeExtend a -> (u - l) * a

  -- pre-enlarged bounds are looked at in the following order:
  --   - concrete bounds from max/boundMin
  --   - inferred bounds from plot envelopes
  --   - backup bounds
  l = fromMaybe asBackupBoundMin $ asBoundMin <|> lI
  u = fromMaybe asBackupBoundMax $ asBoundMax <|> uI
  lI = preview (folded . _1) mInferred
  uI = preview (folded . _2) mInferred

-- | Calculate the scaling for the axis.
--
--   The result returns:
--
--     - The final bounds for the axis
--     - scale to match desired 'scaleAspectRatio'
--     - scale to match desired 'asSizeSpec'
calculateScaling
  :: (HasLinearMap v, Applicative v)
  => v AxisScaling -- ^ axis scaling options
  -> BoundingBox v Double   -- ^ bounding box from the axis plots
  -> (v (Double,Double), Transformation v Double, Transformation v Double)
calculateScaling aScaling bb = (bounds, aspectScaling, sizeScaling) where

  -- final bounds of the axis
  bounds   = calculateBounds <$> aScaling <*> distribute inferred
  inferred = view _Point . uncurry (liftA2 (,)) <$> getCorners bb

  -- the scaling used to meet the desired aspect ratio
  aspectScaling
    -- If any of the aspect ratios are committed we use the aspect ratio from
    -- aScaling. Otherwise no ratios are set, ignore them and scale
    -- such that each axis is the same length
    | anyOf (folded . scaleAspectRatio) isJust aScaling
                = scalingV $ view (scaleAspectRatio . non 1) <$> aScaling
    | otherwise = inv $ scalingV v

  -- scaling used so the axis fits in the size spec
  sizeScaling = requiredScaling szSpec v'
  -- the vector that points from the lower bound to the upper bound of the
  -- axis
  v  = uncurry (flip (-)) <$> bounds
  v' = apply aspectScaling v
  szSpec = view asSizeSpec aScaling

-- | Apply a function if the predicate is true.
whenever :: Bool -> (a -> a) -> a -> a
whenever b f = bool id f b

-- Logarithmic scaling -------------------------------------------------

-- Logarithmic scales are achieved by having 'LinearAxis' or 'LogAxis'
-- for each of the axes. When rendering the plots, they have axes the
-- log scheme. Some plots (like scatter) can easily do this whereas
-- others (like diagram plot) it's nearly impossible for, so they don't
-- bother.
--
-- Support for Log axis still needs a lot of work and debugging.

-- | Should the axis be on a logarithmic scale. The 'Default' is
--   'LinearAxis'.
data LogScale = LinearAxis | LogAxis
  deriving (Show, Eq)

instance Default LogScale where
  def = LinearAxis

-- | Log the number for 'LogAxis', do nothing for 'LinearAxis'.
logNumber :: Floating a => LogScale -> a -> a
logNumber LinearAxis = id
logNumber LogAxis    = log
{-# INLINE logNumber #-}

-- | Transform a point according to the axis scale. Does nothing for
--   linear scales.
logPoint :: (Additive v, Floating n) => v LogScale -> Point v n -> Point v n
logPoint v = _Point %~ liftI2 logNumber v
{-# INLINE logPoint #-}

-- | Deform an object according to the axis scale. Does nothing for
--   linear scales.
-- logDeform :: (InSpace v n a, F.Foldable v, Floating n, Deformable a a)
--           => v LogScale -> a -> a
-- logDeform v
--   | allOf folded (== LinearAxis) v = id
--   | otherwise                      = deform (Deformation $ logPoint v)

