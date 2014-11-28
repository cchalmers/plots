{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiWayIf            #-}

module Plots.Axis.Render where

import           Control.Lens          hiding (lmap, transform, ( # ))
import           Control.Lens.Extras   (is)
import           Data.Typeable
import           Diagrams.Prelude      as D hiding (under, view)
import           Diagrams.TwoD.Text

import           Data.Distributive
import           Data.Foldable
import           Data.Monoid.Recommend

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Types

import           Diagrams.BoundingBox
import           Diagrams.Extra
import           Diagrams.LinearMap

import           Linear                hiding (translation)

class RenderAxis b v n where
  renderAxis :: Axis b v n -> QDiagram b V2 n Any

instance (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Typeable b)
    => RenderAxis b V3 n where
  renderAxis = renderR3Axis

renderR3Axis :: (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Typeable b)
    => Axis b V3 n -> QDiagram b V2 n Any
renderR3Axis a = frame 15
               $ legend
              <> plots
              <> drawAxis ex ey LowerLabels
              <> drawAxis ey ex UpperLabels
              <> drawAxis ez ey LowerLabels
              <> drawAxis ey ez NoLabels
              <> drawBackAxis ez ex NoLabels
              <> drawBackAxis ex ez NoLabels
  where
    plots        = foldMap (plotPlot xs tv l t2) plots'
    drawAxis     = axisOnBasis minPoint xs a tv l t2
    drawBackAxis = axisOnBasis backPoint xs a tv l t2

    minPoint  = P $ fmap fst xs
    backPoint = P $ view <$> V3 _1 _2 _1 <*> xs
    --
    (xs, tv, t2) = workOutScale a
    --
    bb = fromCorners (P . l $ fmap fst xs) (P . l $ fmap snd xs)
    legend = drawLegend bb (a ^. axisLegend) (plots' ^.. traversed)
                        -- (a ^.. axisPlots . traversed . genericPlot)
    --
    plots' = a ^. axisPlots . to applyTheme
    -- TODO: fix this
    applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)
    --
    l = a ^. axisLinearMap

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Plotable (Plot b V2 n) b)
    => RenderAxis b V2 n where
  renderAxis = renderR2Axis

renderR2Axis :: (Typeable b, TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Plotable (Plot b V2 n) b)
  => Axis b V2 n -> QDiagram b V2 n Any
renderR2Axis a = frame 15
               $ legend
              <> plots
              <> drawAxis ex ey LowerLabels
              <> drawAxis ey ex LowerLabels
  where
    plots = foldMap (plot xs tv (a ^. axisLinearMap) t2) plots'
    drawAxis = axisOnBasis origin xs a tv (a ^. axisLinearMap) t2
    --
    (xs, tv, t2) = workOutScale a
    --
    bb = fromCorners (P . apply t2 . apply tv $ fmap fst xs) (P . apply t2 . apply tv $ fmap snd xs)
    legend = drawLegend bb (a ^. axisLegend) (toList plots')
    --
    -- TODO: fix this
    applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)
    plots' = a ^. axisPlots . to applyTheme

data LabelPosition = NoLabels
                   | LowerLabels
                   | UpperLabels
  deriving (Show, Eq, Typeable)

axisOnBasis
  :: forall b v n. (TypeableFloat n, HasLinearMap v, Metric v, Renderable (Path V2 n) b, n ~ N (v n), v ~ V (v n))
  => Point v n        -- start of axis
  -> v (n, n)         -- calculated bounds
  -> Axis b v n       -- axis data
  -> Transformation v n -- transformation to apply to positions of things
  -> (v n -> V2 n)    -- linear map onto R2
  -> T2 n             -- transformation to apply to positions of things
  -> E v              -- direction of axis
  -> E v              -- orthogonal direction of axis
  -> LabelPosition    -- where (if at all) should labels be placed?
  -> QDiagram b V2 n Any   -- resulting axis
axisOnBasis p bs a tv l t2 e eO lp = tickLabels <> axLabels <> ticks <> line <> grid
  where
    tStroke = stroke . transform t2 . lmap l . transform tv

    -- axis labels (x,y etc.)
    axLabels = if null txt || lp == NoLabels
                 then mempty
                 else (axLabelD ^. axisLabelFunction) txt
                         # moveTo p'
                         # applyStyle (axLabelD ^. axisLabelStyle)

      where
        p' = p # over lensP ((el e .~ x) . (el eO .~ y0))
               # papply (translationE eO (negate' labelGap / avgScale t2))
               # papply tv
               # lmap l
               # papply t2
        labelGap = axLabelD ^. axisLabelGap
        txt      = axLabelD ^. axisLabelText
        x = case axLabelD ^. axisLabelPos of
              MiddleAxisLabel -> (x0 + x1) / 2
              LowerAxisLabel  -> x0
              UpperAxisLabel  -> x1
        axLabelD = a ^. axisLabels . el e

    -- tick labels
    tickLabels
      | lp == NoLabels = mempty
      | otherwise = foldMap drawLabels (take 1 ys)
                      # applyStyle (tickLabelsD ^. tickLabelStyle)
      where
        tickLabelsD  = a ^. axisTickLabels . el e
        labelFun     = tickLabelsD ^. tickLabelFunction
        drawLabels y = foldMap f (labelFun majorTickXs b)
          where
            f (x, dia) = place dia p'
              where
                p' = over lensP ((el e .~ x) . (el eO .~ y)) p
                       # papply tv
                       # papply (translationE eO (negate' 15 / avgScale t2))
                       # lmap l
                       # papply t2

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
          where f y = over lensP ((el e .~ x) . (el eO .~ y)) p
        --
        gridD = a ^. axisGridLines ^. el e -- :: GridLines N

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
            [ origin & ep eO -~ d
            , origin & ep eO +~ d ]
              # lmap l -- Note: only works for linear maps
        positionTick tick x = place tick p'
          where
            p' = over lensP ((el e .~ x) . (el eO .~ y)) p
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
         $ map (\x -> over lensP ((el e .~ x) . (el eO .~ y)) p) [x0, x1] :: Path v n

    -- measurements
    b@(x0,x1)  = bs ^. el e :: (n, n)
    yb@(y0,y1) = bs ^. el eO . if lp == UpperLabels
                                 then swapped
                                 else id
    --
    ticksD      = a ^. axisTicks . el e
    majorTickXs = (ticksD ^. majorTicksFun) b
    minorTickXs = (ticksD ^. minorTicksFun) majorTickXs b
    --
    ys       = getAxisLinePos yb lineType
    lineType = a ^. axisLines . el e . axisLineType
    --
    negate' = if lp == UpperLabels
                then id
                else negate


-- Rules for choosing scales:
--   - The default is to have each axis the same length:
--       - for Width and Height specs, this is easy:
--           - adjust each of the bounds so they're the same length
--             (via inverseScale to bound diffs)
--           - work out the uniform scale needed to make it w or h
--           - return inverseScale <> uniform scale
--           - if aspect ratios are set, use this instead of inverse of diffs
--
--  - if width and height are from spec (Dims):
--      - do same with bounds
--      - scale w and h independently
--          - this means aspect ratios are not kept
--          - to do this we need to adjust the bounds (hard for 3d?)
--

workOutScale :: (HasLinearMap v, V (v n) ~ v, Distributive v, OrderedField n, Applicative v)
  => Axis b v n
  -> (v (n,n), Transformation v n, T2 n)
workOutScale a = (enlargedBounds, aspectScaling, specScaling)
 where
    enlargedBounds = workOutUsedBounds
                       aScaling
                       -- disgusting
                       (view lensP . uncurry (liftA2 (,)) <$> getCorners bb)
                       bnd
    V2 x y = l . apply aspectScaling $ v

    -- the vector that points from the lower bound to the upper bound of the
    -- axis
    v = uncurry (flip (-)) <$> enlargedBounds

    aspectScaling
      -- if any of the aspect ratios are committed we use the aspect ratio from
      -- aScaling
      | anyOf (folded . aspectRatio) (is _Commit) aScaling
          = vectorScaling (view (aspectRatio . recommend) <$> aScaling)
      -- otherwise all ratios are just recommend, ignore them and scale such
      -- that each axis is the same length
      | otherwise
          = inv $ vectorScaling v

    specScaling = requiredScaling spec2d (V2 x y)
    -- specScaling = case spec2d of
    --   Absolute -> mempty
    --   Width w  -> scaling (w / x)
    --   Height h -> scaling (h / y)
    --   Dims w h -> scalingX (w / x) <> scalingY (h / y)

    --

    l        = a ^. axisLinearMap
    spec2d   = a ^. axisSize
    aScaling = a ^. axisScaling
    bb       = a ^. axisPlots . folded . plotBoundingBox
    bnd      = a ^. bounds

-- messy tempory fix while stuff is getting worked out
workOutUsedBounds :: (Applicative v, Distributive v, Num n)
  => v (Scaling n) -> Maybe (v (n, n)) -> Bounds v n -> v (n, n)
workOutUsedBounds aScale mBox bnd =
  workOutUsedBound <$> aScale <*> distribute mBox <*> (\(Bounds a) -> a) bnd

workOutUsedBound :: Num n => Scaling n -> Maybe (n, n) -> Bound n -> (n, n)
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

-- utilities

translationE :: (Num n, HasLinearMap v) => E v -> n -> Transformation v n
translationE (E l) x = translation (zero & l .~ x)

ep :: E v -> Lens' (Point v x) x
ep (E l) = lensP . l
{-# INLINE ep #-}

vectorScaling :: (Additive v, Fractional n) => v n -> Transformation v n
vectorScaling v = fromLinear f f
  where f = liftI2 (*) v <-> liftI2 (flip (/)) v

scaleE :: (Additive v, Fractional n) => E v -> n -> Transformation v n
scaleE e s = fromLinear f f
  where f = (el e *~ s) <-> (el e //~ s)

-- old code

-- workOutScale
--   :: (HasLinearMap v, V (v n) ~ v, FoldableWithIndex (E v) v, Distributive v, OrderedField n)
--   => (v n -> V2 n)   -- linear map
--   -> SizeSpec2D n    -- size spec axis should fit in
--   -> AxisScaling v n -- scaling options
--   -> BoundingBox v n -- bounding box of plots
--   -> Bounds v n      -- axis bounds
--   -> (v (n, n), Transformation v n, T2 n)
-- workOutScale l spec2d aScaling bb bnd = (enlargedBounds, aspectScaling, specScaling)
--   where
--     enlargedBounds = workOutUsedBounds
--                        aScaling
--                        -- disgusting
--                        (view lensP . uncurry (liftA2 (,)) <$> getCorners bb)
--                        bnd
--     V2 x y = l . apply aspectScaling $ v
-- 
--     -- the vector that points from the lower bound to the upper bound of the
--     -- axis
--     v = uncurry (flip (-)) <$> enlargedBounds
-- 
--     aspectScaling
--       -- if any of the aspect ratios are committed we use the aspect ratio from
--       -- aScaling
--       | anyOf (folded . aspectRatio) (is _Commit) aScaling
--           = vectorScaling (view (aspectRatio . recommend) <$> aScaling)
--       -- otherwise all ratios are just recommend, ignore them and scale such
--       -- that each axis is the same length
--       | otherwise
--           = inv $ vectorScaling v
-- 
--     specScaling = case spec2d of
--       Absolute -> mempty
--       Width w  -> scaling (w / x)
--       Height h -> scaling (h / y)
--       Dims w h -> scalingX (w / x) <> scalingY (h / y)


getAxisLinePos :: (Num n, Ord n) => (n, n) -> AxisLineType -> [n]
getAxisLinePos (a,b) aType = case aType of
  BoxAxisLine    -> [a, b]
  LeftAxisLine   -> [a]
  MiddleAxisLine -> [if | a > 0     -> a
                        | b < 0     -> b
                        | otherwise -> 0]
  RightAxisLine  -> [b]
  NoAxisLine     -> []


