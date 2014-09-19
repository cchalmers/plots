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
import Data.Typeable
import Diagrams.Prelude   as D hiding (under, view)
import Diagrams.TwoD.Text

-- import Diagrams.Core.Transform

import Data.Monoid.Recommend

import Plots.Axis.Grid
import Plots.Axis.Labels
import Plots.Axis.Ticks
import Plots.Legend
import Plots.Themes
import Plots.Types

import Diagrams.Projections
import Diagrams.ThreeD.Types

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
data AxisLine n = AxisLine
  { _axisLineType  :: AxisLineType
  , _axisArrowOpts :: Maybe (ArrowOpts n)
  } deriving Typeable

makeLenses ''AxisLine

type AxisLines v n = v (AxisLine n)

instance Default (AxisLine n) where
  def = AxisLine
          { _axisLineType  = def
          , _axisArrowOpts = def
          }

-- Scaling

type AspectRatio v n = v n

data ScaleMode = AutoScale
               | NoScale
               | Stretch
               | UniformScale UniformScaleStrategy

data UniformScaleStrategy = AutoUniformScale
                          | UnitOnly
                          | ChangeVerticalLimits
                          | ChangeHorizontalLimits

data Scaling n = Scaling
  { _aspectRatio        :: Recommend n
  , _axisPostScale      :: Maybe n
  , _axisScaleMode      :: ScaleMode
  , _enlargeAxisLimits  :: Maybe (Recommend n)
  }


makeLenses ''Scaling

type AxisScaling v n = v (Scaling n)

instance Fractional n => Default (Scaling n) where
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
data Axis b v n = Axis
  
  { -- These lenses are not being exported, they're just here for instances.
    _axisAxisBounds :: Bounds v n

  -- These lenses are exported.
  , _axisGridLines  :: AxisGridLines v n
  , _axisLabels     :: AxisLabels b v n
  , _axisLegend     :: Legend b n
  , _axisLinearMap  :: v n -> V2 n
  , _axisLines      :: AxisLines v n
  , _axisPlots      :: [Plot b v n]
  , _axisScaling    :: AxisScaling v n
  , _axisSize       :: SizeSpec2D n
  , _axisTheme      :: Theme b n
  , _axisTickLabels :: AxisTickLabels b v n
  , _axisTicks      :: AxisTicks v n
  , _axisTitle      :: Maybe String
  } deriving Typeable

makeLenses ''Axis

type instance V (Axis b v n) = v
type instance N (Axis b v n) = n

axisLine :: E v -> Lens' (Axis b v n) (AxisLine n)
axisLine (E l) = axisLines . l

instance HasBounds (Axis b v n) where
  bounds = axisAxisBounds

-- R2 axis

instance (DataFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b) => Default (Axis b V2 n) where
  def = Axis
          { _axisTitle      = Nothing
          , _axisSize       = Width 300
          , _axisPlots      = []
          , _axisLegend     = def
          , _axisTheme      = coolTheme
          , _axisLinearMap  = id
          , _axisAxisBounds = Bounds $ pure def
          , _axisGridLines  = pure def
          , _axisLabels     = pure def
          , _axisScaling    = pure def
          , _axisTickLabels = pure def
          , _axisTicks      = pure def
          , _axisLines      = pure def
          }


-- renderR2Axis :: (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Plotable (Plot b V2 n) b)
--   => Axis b V2 n -> Diagram b V2 n
-- renderR2Axis a = frame 15
--                $ legend
--               <> plots
--               <> drawAxis ex ey LowerLabels
--               <> drawAxis ey ex LowerLabels
--   where
--     plots = foldMap (plot xs tv (a ^. axisLinearMap) t2) plots'
--     drawAxis = axisOnBasis origin xs a tv (a ^. axisLinearMap) t2
--     --
--     (xs, tv, t2) = workOutScale
--                  (a ^. axisLinearMap)
--                  (a ^. axisSize)
--                  (a ^. axisScaling)
--                  (a ^. axisPlots . traversed . plotBoundingBox)
--                  (a ^. bounds)
--     --
--     legend = drawLegend (a ^. axisLegend) (toList plots')
--     --
--     -- TODO: fix this
--     applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)
--     plots' = a ^. axisPlots . to applyTheme
    

-- R3 Axis

instance (DataFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b) => Default (Axis b V3 n) where
  def = Axis
          { _axisTitle      = Nothing
          , _axisSize       = Width 300
          , _axisPlots      = []
          , _axisLegend     = def
          , _axisTheme      = coolTheme
          , _axisLinearMap  = isometricProjection
          , _axisAxisBounds = Bounds $ pure def
          , _axisGridLines  = pure def
          , _axisLabels     = pure def
          , _axisScaling    = pure def
          , _axisTickLabels = pure def
          , _axisTicks      = pure def
          , _axisLines      = pure def
          }

-- Drawing the axis

getAxisLinePos :: (Num n, Ord n) => (n, n) -> AxisLineType -> [n]
getAxisLinePos (a,b) aType = case aType of
  BoxAxisLine    -> [a, b]
  LeftAxisLine   -> [a]
  MiddleAxisLine -> [if | a > 0     -> a
                        | b < 0     -> b
                        | otherwise -> 0]
  RightAxisLine  -> [b]
  NoAxisLine     -> []

-- renderR3Axis :: (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Plotable (Plot b V3 n) b)
--   => Axis b V3 n -> Diagram b V2 n
-- renderR3Axis a = frame 15
--                $ legend
--               <> plots
--               <> drawAxis ex ey LowerLabels
--               <> drawAxis ey ex UpperLabels
--               <> drawAxis ez ey LowerLabels
--               <> drawAxis ey ez NoLabels
--               <> drawBackAxis ez ex NoLabels
--               <> drawBackAxis ex ez NoLabels 
--   where
--     plots = foldMap (plot xs tv (a ^. axisLinearMap) t2) plots'
--     drawAxis = axisOnBasis minPoint xs a tv (a ^. axisLinearMap) t2
--     drawBackAxis = axisOnBasis backPoint xs a tv (a ^. axisLinearMap) t2
-- 
--     minPoint  = fmap fst xs
--     backPoint = view <$> V3 _1 _2 _1 <*> xs
--     --
--     (xs, tv, t2) = workOutScale
--                  (a ^. axisLinearMap)
--                  (a ^. axisSize)
--                  (a ^. axisScaling)
--                  (a ^. axisPlots . traversed . plotBoundingBox)
--                  (a ^. bounds)
--     --
--     legend = drawLegend (a ^. axisLegend) (plots' ^.. traversed)
--                         -- (a ^.. axisPlots . traversed . genericPlot)
--     --
--     plots' = a ^. axisPlots . to applyTheme
--     -- TODO: fix this
--     applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)
-- 
-- data LabelPosition = NoLabels
--                    | LowerLabels
--                    | UpperLabels
--   deriving (Show, Eq, Typeable)
-- 
-- axisOnBasis
--   :: forall b v n. (OrderedField n, HasLinearMap v, Metric v, Renderable (Path V2 n) b)
--   => Point v n        -- start of axis
--   -> v (n, n)         -- calculated bounds
--   -> Axis b v n       -- axis data
--   -> Transformation v n -- transformation to apply to positions of things
--   -> (v n -> V2 n)    -- linear map onto R2
--   -> T2 n             -- transformation to apply to positions of things
--   -> E v              -- direction of axis
--   -> E v              -- direction normal to axis
--   -> LabelPosition    -- where (if at all) should labels be placed?
--   -> Diagram b V2 n   -- resulting axis
-- axisOnBasis p bs a tv l t2 e eO lp = tickLabels <> axLabels <> grid <> ticks <> line
--   where
--     tStroke = stroke . transform t2 . lmap l . transform tv
-- 
--     -- axis labels (x,y etc.)
--     axLabels = if null txt || lp == NoLabels
--                  then mempty
--                  else (axLabelD ^. axisLabelFunction) txt
--                          # moveTo p'
--                          # applyStyle (axLabelD ^. axisLabelStyle)
-- 
--       where
--         p' = p # ((el e .~ x) . (el eO .~ y0))
--                # transform (translationE eO (negate' labelGap / avgScale t2))
--                # transform tv
--                # lmap l
--                # transform t2
--         labelGap = axLabelD ^. axisLabelGap
--         txt      = axLabelD ^. axisLabelText
--         x = case axLabelD ^. axisLabelPos of
--               MiddleAxisLabel -> (x0 + x1) / 2
--               LowerAxisLabel  -> x0
--               UpperAxisLabel  -> x1
--         axLabelD = a ^. axisLabels . el e
-- 
--     -- tick labels
--     tickLabels
--       | lp == NoLabels = mempty
--       | otherwise = foldMap drawLabels (take 1 ys)
--                       # applyStyle (tickLabelsD ^. tickLabelStyle)
--       where
--         tickLabelsD  = a ^. axisTickLabels . el e
--         labelFun     = tickLabelsD ^. tickLabelFunction
--         drawLabels y = foldMap f (labelFun majorTickXs b)
--           where
--             f (x, dia) = place dia p'
--               where
--                 p' = ((el e .~ x) . (el eO .~ y)) p
--                        # transform tv
--                        # transform (translationE eO (negate' 15 / avgScale t2))
--                        # lmap l
--                        # transform t2
-- 
--     -- grid
--     grid = majorLines <> minorLines
--       where
--         majorLines = foldMap mkGridLine majorGridXs
--                        # tStroke
--                        # applyStyle (gridD ^. majorGridStyle)
--         majorGridXs = (gridD ^. majorGridF) majorTickXs b
--         --
--         minorLines = foldMap mkGridLine minorGridXs
--                        # tStroke
--                        # applyStyle (gridD ^. minorGridStyle)
--         minorGridXs = (gridD ^. minorGridF) minorTickXs b
--         -- --
--         mkGridLine x = pathFromVertices [f y0, f y1]
--           where f y = ((el e .~ x) . (el eO .~ y)) p
--         --
--         gridD = a ^. axisGridLines ^. el e -- :: GridLines N
-- 
--     -- ticks
--     ticks = foldMap drawTicks ys
-- 
--     drawTicks y = majorTicks <> minorTicks
--       where
--         majorTicks = foldMap (positionTick majorTick) majorTickXs
--                        # stroke
--                        # applyStyle (ticksD ^. majorTickStyle)
--         --
--         minorTicks = foldMap (positionTick minorTick) minorTickXs
--                        # stroke
--                        # applyStyle (ticksD ^. minorTickStyle)
--         --
--         minorTick = middleTick (ticksD ^. minorTickLength)
--         majorTick = middleTick (ticksD ^. majorTickLength)
--         --
--         middleTick d =
--           pathFromVertices
--             [ origin & el eO -~ d
--             , origin & el eO +~ d ]
--               # lmap l -- Note: only works for linear maps
--         positionTick tick x = place tick p'
--           where
--             p' = ((el e .~ x) . (el eO .~ y)) p
--                    # transform tv
--                    # lmap l
--                    # transform t2
-- 
--     -- axis lines
--     line = foldMap mkline ys -- merge with ticks?
--              # transform tv
--              # lmap l
--              # transform t2
--              # stroke
--              # applyStyle (a ^. axisLine e . axisArrowOpts . _Just . shaftStyle)
--       where
--         -- TODO: Arrow for R3
--         mkline y = pathFromVertices
--          $ map (\x -> ((el e .~ x) . (el eO .~ y)) p)
--                [x0, x1]
-- 
--     -- measurements
--     b@(x0,x1) = bs ^. el e
--     yb@(y0,y1) = bs ^. el eO . if lp == UpperLabels
--                               then swapped
--                               else id
--     --
--     ticksD      = a ^. axisTicks . el e
--     majorTickXs = (ticksD ^. majorTicksFun) b
--     minorTickXs = (ticksD ^. minorTicksFun) majorTickXs b
--     --
--     ys       = getAxisLinePos yb lineType
--     lineType = a ^. axisLines . el e . axisLineType
--     --
--     negate' = if lp == UpperLabels
--                 then id
--                 else negate
-- 
-- translationE :: (Num n, HasLinearMap v)
--   => E v -> n -> Transformation v n
-- translationE e x = translation (pure 0 & el e .~ x)
-- 
-- 
-- -- Rules for choosing scales:
-- --   - The default is to have each axis the same length:
-- --       - for Width and Height specs, this is easy:
-- --           - adjust each of the bounds so they're the same length
-- --             (via inverseScale to bound diffs)
-- --           - work out the uniform scale needed to make it w or h
-- --           - return inverseScale <> uniform scale
-- --           - if aspect ratios are set, use this instead of inverse of diffs
-- --
-- --  - if width and height are from spec:
-- --      - do same with bounds
-- --      - scale w and h independently
-- --          - this means aspect ratios are not kept
-- --          - to do this we need to adjust the bounds (hard for 3d?)
-- --
-- 
-- workOutScale
--   :: (HasLinearMap v, V (v n) ~ v)
--   => (v n -> V2 n)   -- linear map
--   -> SizeSpec2D n  -- size spec axis should fit in
--   -> AxisScaling v n -- scaling options
--   -> BoundingBox v n -- bounding box of plots
--   -> Bounds v n      -- axis bounds
--   -> (v (n, n), Transformation v n, T2 n)
-- workOutScale l spec2d aScaling mBB bnd
--     = (enlargedBounds, aspectScaling, specScaling)
--   where
--     enlargedBounds = workOutUsedBounds
--                        aScaling
--                        -- disgusting
--                        (uncurry (liftA2 (,))
--                          <$> over (_Just . both)
--                                   (getCorners mBB))
--                        bnd
--     (x,y) = unr2 . over both l . apply aspectScaling $ v
--               -- (view diagramsCoord $ uncurry (flip (-)) <$> enlargedBounds)
-- 
--     -- the vector that points from the lower bound to the upper bound of the 
--     -- axis
--     v = uncurry (flip (-)) <$> enlargedBounds
-- 
--     aspectScaling
--       -- if any of the aspect ratios are committed we use the aspect ratio from 
--       -- aScaling
--       | anyOf (traversed . aspectRatio) (is _Commit) aScaling
--           = vectorScaling (view (aspectRatio . recommend) <$> aScaling)
--       -- otherwise all ratios are just recommend, ignore them and scale such 
--       -- that each axis is the same length
--       | otherwise
--           = inv $ vectorScaling v
-- 
--     specScaling   = case spec2d of
--       Absolute -> mempty
--       Width w  -> scaling (w / x)
--       Height h -> scaling (h / y)
--       Dims w h -> scalingX (w / x) <> scalingY (h / y)
-- 
-- 
-- 
-- vectorScaling :: (Fractional n, FoldableWithIndex (E v) v, HasLinearMap v)
--   => v n -> Transformation v n
-- vectorScaling = ifoldMap scaleE
-- 
-- 
-- scaleE :: (HasLinearMap v, Fractional n)
--   => E v -> n -> Transformation v n
-- scaleE e s = fromLinear f f
--   where
--     f = (el e *~ s) <-> (el e //~ s)
-- 
-- -- messy tempory fix while stuff is getting worked out
-- workOutUsedBounds :: (Applicative t, Distributive t)
--   => v (Scaling n) -> Maybe (v (n, n)) -> Bounds v n -> v (n, n)
-- workOutUsedBounds aScale mBox bnd =
--   workOutUsedBound <$> aScale <*> distribute mBox <*> bnd
-- 
-- workOutUsedBound :: Scaling n -> Maybe (n, n) -> Bound n -> (n, n)
-- workOutUsedBound aScale mBox (Bound rL rU) = enlarged
--   where
--     -- TODO: - make better
--     --       - seperate enlarge axis for lower and upper bounds
--     --       - absolute units as well as scale factor (requires refactoring)
--     enlarged = case aScale ^. enlargeAxisLimits of
--       Nothing            -> (l', u')
-- 
--       -- committed enlargements enlarge all bounds
--       Just (Commit s)    -> (l' - s * r', u' + s * r')
-- 
--       -- recommended enlargements only apply to non-committed bounds
--       Just (Recommend s) -> ( if isn't _Commit rL
--                                then l' - s * r'
--                                else l'
--                             , if isn't _Commit rU
--                                 then u' + s * r'
--                                 else u'
--                             ) -- I'm sure there's a better way
-- 
--     -- unenlarged bounds
-- 
--     r' = u' - l'
--      -- mBox is the concatination of bounding boxes of all axis plots
--     (l', u') = case mBox of
-- 
--       -- infered bounds (from bounding box) only used for non-committed bounds
--       Just (l,u) -> (fromCommit l rL, fromCommit u rU)
-- 
--       -- recommended bounds are used when no infered bounds exist
--       Nothing    -> (getRecommend rL, getRecommend rU)
-- 
