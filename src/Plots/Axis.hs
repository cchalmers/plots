{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Plots.Axis where

import Control.Lens       hiding (lmap, transform, ( # ))
import Data.Default
import Data.Foldable
import Data.Typeable
import Diagrams.Prelude   as D hiding (under, view)
import Diagrams.TwoD.Text

-- import Diagrams.Core.Transform

import Data.Monoid.Recommend
import Diagrams.BoundingBox
import Diagrams.Extra
import Diagrams.LinearMap
import           Data.Distributive
import qualified Diagrams.Prelude2 as P2

import Plots.Axis.Grid
import Plots.Axis.Labels
import Plots.Axis.Ticks
import Plots.Legend
import Plots.Themes
import Plots.Types

import Diagrams.Coordinates.Traversals
import Diagrams.Projections
import Diagrams.ThreeD.Types
import Linear                          (E, el, ex, ey, ez)

import Control.Lens.Extras (is)

import Data.LinearMap

-- Lines types

-- | Where axis line for coordinate should be drawn.
data AxisLineType
  = BoxAxisLine
  | LeftAxisLine
  | MiddleAxisLine
  | RightAxisLine
  | NoAxisLine
  deriving (Show, Eq, Typeable)

instance Default AxisLineType where
  def = BoxAxisLine

-- | Information about position and style of axis lines.
data AxisLine = AxisLine
  { _axisLineType  :: AxisLineType
  , _axisArrowOpts :: Maybe ArrowOpts
  } deriving Typeable

makeLenses ''AxisLine

type AxisLines v = T v AxisLine

instance Default AxisLine where
  def = AxisLine
          { _axisLineType  = def
          , _axisArrowOpts = def
          }

-- Scaling

type AspectRatio v = T v Double

data ScaleMode = AutoScale
               | NoScale
               | Stretch
               | UniformScale UniformScaleStrategy

data UniformScaleStrategy = AutoUniformScale
                          | UnitOnly
                          | ChangeVerticalLimits
                          | ChangeHorizontalLimits

data Scaling = Scaling
  { _aspectRatio        :: Recommend Double
  , _axisPostScale      :: Maybe Double
  , _axisScaleMode      :: ScaleMode
  , _enlargeAxisLimits  :: Maybe (Recommend Double)
  }


makeLenses ''Scaling

type AxisScaling v = T v Scaling

instance Default Scaling where
  def = Scaling
          { _aspectRatio       = Recommend 1
          , _axisPostScale     = Nothing
          , _axisScaleMode     = AutoScale
          , _enlargeAxisLimits = Just $ Recommend 0.1
          }

-- axis data type

-- | Axis is the data type that holds all the nessessary information to render
--   a plot. The idea is to use one of the default axis, customise, add plots
--   and render using @drawAxis@.
data Axis b v = Axis
  
  { -- These lenses are not being exported, they're just here for instances.
    _axisAxisBounds :: Bounds v

  -- These lenses are exported.
  , _axisGridLines  :: AxisGridLines v
  , _axisLabels     :: AxisLabels b v
  , _axisLegend     :: Legend b
  , _axisLinearMap  :: v :-* R2
  , _axisLines      :: AxisLines v
  , _axisPlots      :: [Plot b v]
  , _axisScaling    :: AxisScaling v
  , _axisSize       :: SizeSpec2D
  , _axisTheme      :: Theme b
  , _axisTickLabels :: AxisTickLabels b v
  , _axisTicks      :: AxisTicks v
  , _axisTitle      :: Maybe String
  } deriving Typeable

makeLenses ''Axis

type instance V (Axis b v) = v

axisLine :: E (T v) -> Lens' (Axis b v) AxisLine
axisLine e = axisLines . el e

instance HasBounds (Axis b v) where
  bounds = axisAxisBounds

-- R2 axis

instance (Renderable Text b, Renderable (Path R2) b) => Default (Axis b R2) where
  def = Axis
          { _axisTitle      = Nothing
          , _axisSize       = Width 300
          , _axisPlots      = []
          , _axisLegend     = def
          , _axisTheme      = coolTheme
          , _axisLinearMap  = idL
          , _axisAxisBounds = pure def
          , _axisGridLines  = pure def
          , _axisLabels     = pure def
          , _axisScaling    = pure def
          , _axisTickLabels = pure def
          , _axisTicks      = pure def
          , _axisLines      = pure def
          }


renderR2Axis :: (Renderable (Path R2) b, Renderable Text b, Plotable (Plot b R2) b)
  => Axis b R2 -> Diagram b R2
renderR2Axis a = P2.frame 15
               $ legend
              <> plots
              <> drawAxis ex ey
              <> drawAxis ey ex
  where
    plots = foldMap (plot xs tv (a ^. axisLinearMap) t2) (a ^. axisPlots . to applyTheme)
    drawAxis = axisOnBasis origin xs a tv (a ^. axisLinearMap) t2
    --
    (xs, tv, t2) = workOutScale
                 (a ^. axisLinearMap)
                 (a ^. axisSize)
                 (a ^. axisScaling)
                 (a ^. axisPlots . traversed . plotBoundingBox)
                 (a ^. bounds)
    --
    legend = drawLegend (a ^. axisLegend)
                        (a ^.. axisPlots . traversed . genericPlot)
    --
    -- TODO: fix this
    applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)

-- R3 Axis

instance (Renderable Text b, Renderable (Path R2) b) => Default (Axis b R3) where
  def = Axis
          { _axisTitle      = Nothing
          , _axisSize       = Width 300
          , _axisPlots      = []
          , _axisLegend     = def
          , _axisTheme      = coolTheme
          , _axisLinearMap  = linear isometricProjection
          , _axisAxisBounds = pure def
          , _axisGridLines  = pure def
          , _axisLabels     = pure def
          , _axisScaling    = pure def
          , _axisTickLabels = pure def
          , _axisTicks      = pure def
          , _axisLines      = pure def
          }

-- Drawing the axis

getAxisLinePos :: (Double, Double) -> AxisLineType -> [Double]
getAxisLinePos (a,b) aType = case aType of
  BoxAxisLine    -> [a, b]
  LeftAxisLine   -> [a]
  MiddleAxisLine -> [if | a > 0     -> a
                        | b < 0     -> b
                        | otherwise -> 0]
  RightAxisLine  -> [b]
  NoAxisLine     -> []

renderR3Axis :: (Renderable (Path R2) b, Renderable Text b, Plotable (Plot b R3) b)
  => Axis b R3 -> Diagram b R2
renderR3Axis a = P2.frame 15
               $ legend
              <> plots
              <> drawAxis ex ey
              <> drawAxis ey ex
              <> drawAxis ez ey
  where
    plots = foldMap (plot xs tv (a ^. axisLinearMap) t2) (a ^. axisPlots . to applyTheme)
    drawAxis = axisOnBasis minPoint xs a tv (a ^. axisLinearMap) t2
    minPoint = view (from traversablePoint) $ fmap fst xs
    --
    (xs, tv, t2) = workOutScale
                 (a ^. axisLinearMap)
                 (a ^. axisSize)
                 (a ^. axisScaling)
                 (a ^. axisPlots . traversed . plotBoundingBox)
                 (a ^. bounds)
    --
    legend = drawLegend (a ^. axisLegend)
                        (a ^.. axisPlots . traversed . genericPlot)
    --
    -- TODO: fix this
    applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)


axisOnBasis
  :: forall v t b. (t ~ T v,
      Scalar v ~ Double,
      HasLinearMap v,
      InnerSpace v,
      TraversableCoordinate v,
      Renderable (Path R2) b)
  => Point v          -- start of axis
  -> T v (Double, Double) -- calculated bounds
  -> Axis b v         -- axis data
  -> Transformation v -- transformation to apply to positions of things
  -> (v :-* R2)       -- linear map onto R2
  -> Transformation R2 -- transformation to apply to positions of things
  -> E t              -- direction of axis
  -> E t              -- direction normal to axis
  -> Diagram b R2     -- resulting axis
axisOnBasis p bs a tv l t2 e eO = tickLabels <> axLabels <> grid <> ticks <> line
  where
    tStroke = stroke . transform t2 . lmap l . transform tv

    -- axis labels (x,y etc.)
    axLabels = if null txt
                 then mempty
                 else (axLabelD ^. axisLabelFunction) txt
                         # moveTo p'
                         # applyStyle (axLabelD ^. axisLabelStyle)

      where
        p' = p # over traversablePoint ((el e .~ x) . (el eO .~ y0))
               # transform (translationE eO (- labelGap / avgScale t2))
               # transform tv
               # lmap l
               # transform t2
        labelGap = axLabelD ^. axisLabelGap
        txt      = axLabelD ^. axisLabelText
        x = case axLabelD ^. axisLabelPos of
              MiddleAxisLabel -> (x0 + x1) / 2
              LowerAxisLabel  -> x0
              UpperAxisLabel  -> x1
        axLabelD = a ^. axisLabels . el e

    -- tick labels
    tickLabels = foldMap drawLabels (take 1 ys)
                   # applyStyle (tickLabelsD ^. tickLabelStyle)
      where
        tickLabelsD  = a ^. axisTickLabels . el e
        labelFun     = tickLabelsD ^. tickLabelFunction
        drawLabels y = foldMap f (labelFun majorTickXs b)
          where
            f (x, dia) = place dia p'
              where
                p' = over traversablePoint ((el e .~ x) . (el eO .~ y)) p
                       # transform tv
                       # transform (translationE eO (-8 / avgScale t2))
                       # lmap l
                       # transform t2

    -- grid
    grid = majorLines <> minorLines
      where
        majorLines = foldMap mkGridLine majorGridXs
                       # tStroke
                       # applyStyle (gridD ^. majorGridStyle)
        majorGridXs = (gridD ^. majorGridF) majorTickXs b
        --
        minorLines = foldMap mkGridLine minorGridXs
                       # tStroke
                       # applyStyle (gridD ^. minorGridStyle)
        minorGridXs = (gridD ^. minorGridF) minorTickXs b
        -- --
        mkGridLine x = pathFromVertices [f y0, f y1]
          where f y = over traversablePoint ((el e .~ x) . (el eO .~ y)) p
        --
        gridD = a ^. axisGridLines ^. el e :: GridLines

    -- ticks
    ticks = foldMap drawTicks ys

    drawTicks y = majorTicks <> minorTicks
      where
        majorTicks = foldMap (positionTick majorTick) majorTickXs
                       # stroke
                       # applyStyle (ticksD ^. majorTickStyle)
        --
        minorTicks = foldMap (positionTick minorTick) minorTickXs
                       # stroke
                       # applyStyle (ticksD ^. minorTickStyle)
        --
        minorTick = middleTick (ticksD ^. minorTickLength)
        majorTick = middleTick (ticksD ^. majorTickLength)
        --
        middleTick d =
          pathFromVertices
            [ origin & traversablePoint . el eO -~ d
            , origin & traversablePoint . el eO +~ d ]
              # lmap l -- Note: only works for linear maps
        positionTick tick x = place tick p'
          where
            p' = over traversablePoint ((el e .~ x) . (el eO .~ y)) p
                   # transform tv
                   # lmap l
                   # transform t2

    -- axis lines
    line = foldMap mkline ys -- merge with ticks?
             # transform tv
             # lmap l
             # transform t2
             # stroke
             # applyStyle (a ^. axisLine e . axisArrowOpts . _Just . shaftStyle)
      where
        -- TODO: Arrow for R3
        mkline y = pathFromVertices
         $ map (\x -> over traversablePoint ((el e .~ x) . (el eO .~ y)) p)
               [x0, x1]

    -- measurements
    b@(x0,x1) = bs ^. el e
    (y0,y1) = bs ^. el eO
    --
    ticksD      = a ^. axisTicks . el e
    majorTickXs = (ticksD ^. majorTicksFun) b
    minorTickXs = (ticksD ^. minorTicksFun) majorTickXs b
    --
    ys       = getAxisLinePos (bs ^. el eO) lineType
    lineType = a ^. axisLines . el e . axisLineType
    --

translationE :: (HasLinearMap v, TraversableCoordinate v)
  => E (T v) -> Double -> Transformation v
translationE e x = translation (view diagramsCoord $ pure 0 & el e .~ x)


-- Rules for choosing scales:
--   - The default is to have each axis the same length:
--       - for Width and Height specs, this is easy:
--           - adjust each of the bounds so they're the same length
--             (via inverseScale to bound diffs)
--           - work out the uniform scale needed to make it w or h
--           - return inverseScale <> uniform scale
--           - if aspect ratios are set, use this instead of inverse of diffs
--
--  - if width and height are from spec:
--      - do same with bounds
--      - scale w and h independently
--          - this means aspect ratios are not kept
--          - to do this we need to adjust the bounds (hard for 3d?)
--

workOutScale
  :: (Scalar v ~ Double, TraversableCoordinate v, HasLinearMap v, V v ~ v)
  => v :-* R2      -- linear map
  -> SizeSpec2D    -- size spec axis should fit in
  -> AxisScaling v -- scaling options
  -> BoundingBox v -- bounding box of plots
  -> Bounds v      -- axis bounds
  -> (T v (Double, Double), Transformation v, T2)
workOutScale l spec2d aScaling mBB bnd
    = (enlargedBounds, aspectScaling, specScaling)
  where
    enlargedBounds = workOutUsedBounds
                       aScaling
                       -- disgusting
                       (uncurry (liftA2 (,))
                         <$> over (_Just . both)
                                  (view traversablePoint)
                                  (getCorners mBB))
                       bnd
    (x,y) = unr2 . lapply l . apply aspectScaling $ view diagramsCoord v
              -- (view diagramsCoord $ uncurry (flip (-)) <$> enlargedBounds)

    -- the vector that points from the lower bound to the upper bound of the 
    -- axis
    v = uncurry (flip (-)) <$> enlargedBounds

    aspectScaling
      -- if any of the aspect ratios are committed we use the aspect ratio from 
      -- aScaling
      | anyOf (traversed . aspectRatio) (is _Commit) aScaling
          = vectorScaling (view (aspectRatio . recommend) <$> aScaling)
      -- otherwise all ratios are just recommend, ignore them and scale such 
      -- that each axis is the same length
      | otherwise
          = inv $ vectorScaling v

    specScaling   = case spec2d of
      Absolute -> mempty
      Width w  -> scaling (w / x)
      Height h -> scaling (h / y)
      Dims w h -> scalingX (w / x) <> scalingY (h / y)

type instance V (Plot b v) = v



vectorScaling :: (HasLinearMap v, TraversableCoordinate v)
  => T v Double -> Transformation v
vectorScaling = ifoldMap scaleE


scaleE :: (HasLinearMap v, TraversableCoordinate v)
  => E (T v) -> Double -> Transformation v
scaleE e s = fromLinear f f
  where
    f = (traversableCoord . el e *~ s) <-> (traversableCoord . el e //~ s)

-- messy tempory fix while stuff is getting worked out
workOutUsedBounds :: (Applicative t, Distributive t)
  => t Scaling -> Maybe (t (Double, Double)) -> t Bound -> t (Double, Double)
workOutUsedBounds aScale mBox bnd =
  workOutUsedBound <$> aScale <*> distribute mBox <*> bnd

workOutUsedBound :: Scaling -> Maybe (Double, Double) -> Bound -> (Double, Double)
workOutUsedBound aScale mBox (Bound rL rU) = enlarged
  where
    -- TODO: - make better
    --       - seperate enlarge axis for lower and upper bounds
    --       - absolute units as well as scale factor (requires refactoring)
    enlarged = case aScale ^. enlargeAxisLimits of
      Nothing            -> (l', u')

      -- committed enlargements enlarge all bounds
      Just (Commit s)    -> (l' - s * r', u' + s * r')

      -- recommended enlargements only apply to non-committed bounds
      Just (Recommend s) -> ( if isn't _Commit rL
                               then l' - s * r'
                               else l'
                            , if isn't _Commit rU
                                then u' + s * r'
                                else u'
                            ) -- I'm sure there's a better way

    -- unenlarged bounds

    r' = u' - l'
     -- mBox is the concatination of bounding boxes of all axis plots
    (l', u') = case mBox of

      -- infered bounds (from bounding box) only used for non-committed bounds
      Just (l,u) -> (fromCommit l rL, fromCommit u rU)

      -- recommended bounds are used when no infered bounds exist
      Nothing    -> (getRecommend rL, getRecommend rU)

