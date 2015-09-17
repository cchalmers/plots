{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Render
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Low level module containing functions for rendering different types
-- of axis.
--
----------------------------------------------------------------------------
module Plots.Axis.Render where

import           Control.Lens               hiding (lmap, transform, ( # ))
import           Control.Lens.Extras        (is)
import           Data.Distributive
import           Data.Foldable
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.BoundingBox
import           Diagrams.Prelude           as D hiding (under, view)
import           Diagrams.TwoD.Text
import           Linear                     hiding (translation)

import           Diagrams.Coordinates.Polar

import           Plots.Axis
-- import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Style
import           Plots.Axis.ColourBar
import           Plots.Types
import           Plots.Utils

class RenderAxis b v n where
  renderAxis :: Axis b v n -> QDiagram b (BaseSpace v) n Any


-- instance (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Typeable b)
--     => RenderAxis b V3 n where
--   renderAxis = renderR3Axis

-- renderR3Axis :: (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b, Typeable b)
--     => Axis b V3 n -> QDiagram b V2 n Any
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
--     plots        = foldMap (renderPlot xs t) plots'
--     drawAxis     = axisOnBasis minPoint xs a tv l t2
--     drawBackAxis = axisOnBasis backPoint xs a tv l t2

--     minPoint  = P $ fmap fst xs
--     backPoint = P $ view <$> V3 _1 _2 _1 <*> xs
--     --
--     (xs, tv, t2) = workOutScale a
--     --
--     bb = fromCorners (P . l $ fmap fst xs) (P . l $ fmap snd xs)
--     legend = drawLegend bb (a ^. axisLegend) (plots' ^.. traversed)
--                         -- (a ^.. axisPlots . traversed . genericPlot)
--     --
--     plots' = a ^. axisPlots . to applyTheme
--     -- TODO: fix this
--     applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)
--     --
--     l = a ^. axisLinearMap

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b,
          Renderable (Text n) b)
    => RenderAxis b V2 n where
  renderAxis = renderR2Axis

renderR2Axis :: (Typeable b, TypeableFloat n, Renderable (Path V2 n) b,
                 Renderable (Text n) b)
  => Axis b V2 n -> QDiagram b V2 n Any
renderR2Axis a = frame 40
               $ legend
              <> cBar
              <> drawAxis ex ey LowerLabels
              <> drawAxis ey ex LeftLabels
              <> plots
  where
    spec = AxisSpec xs t (a^.axisScale) (a ^. axisColourMap)
    plots    = foldMap (uncurry $ renderPlotable spec) plots'
    drawAxis = axisOnBasis origin xs a (a^.axisScale) t
    --
    (xs, tv, t') = workOutScale (boundingBox $ map fst plots') a
    t = tv <> t'
    --
    bb = fromCorners (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    legend = drawLegend bb (a ^. axisLegend) (toList plots')
    --

    -- The colour bar
    --         & cbExtent .~ ex'
    -- ex' = orient (cbo ^. cbOrientation) (V2 (width bb) 15) (V2 15 (height bb))
    cBar = addColourBar bb (a^.colourBar) (a ^. axisColourMap) (0,1)
    --

    -- render the plots
    -- preparePlots :: [ModifiedPlot b v n] -> [(Plot b v n, PlotProperties b v n)]
    preparePlots =
      zipWith (\theme p' -> modifyPlot p' (startingProperties theme))
              (a ^.. axisStyle . axisStyles)
    plots'     = a ^. axisPlots . to preparePlots

data LabelPosition
  = NoLabels
  | LowerLabels
  | LeftLabels
  | RightLabels
  | UpperLabels
  deriving (Show, Eq, Typeable)

axisOnBasis
  :: forall b v n. (v ~ V2, TypeableFloat n, HasLinearMap v, Metric v,
                    Renderable (Path V2 n) b, n ~ N (v n), v ~ V (v n), OrderedField n)
  => Point v n        -- start of axis
  -> v (n, n)         -- calculated bounds
  -> Axis b v n       -- axis data
  -> v AxisScale      -- log scale?
  -> T2 n             -- transformation to apply to positions of things
  -> E v              -- direction of axis
  -> E v              -- orthogonal direction of axis
  -> LabelPosition    -- where (if at all) should labels be placed?
  -> QDiagram b V2 n Any   -- resulting axis
axisOnBasis p bs a ls t e eO lp = tickLabels <> axLabels <> ticks <> line <> grid
  where
    tStroke = stroke . transform t

    -- axis labels (x,y etc.)
    axLabels = if null txt || lp == NoLabels
                 then mempty
                 else (axLabelD ^. axisLabelFunction) txtAlign txt
                         # moveTo p'
                         # applyStyle (axLabelD ^. axisLabelStyle)
      where
        p' = p & ep e  .~ x
               & ep eO .~ y0
               -- & logPoint ls
               & coscale
               & papply t
               & ep eO +~ negate' labelGap
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
      | otherwise = foldMap drawLabels (map snd $ take 1 ys)
                      # applyStyle (tickLabelsD ^. tickLabelStyle)
      where
        tickLabelsD  = a ^. axisTickLabels . el e
        labelFun     = tickLabelsD ^. tickLabelFun
        drawLabels y = foldMap f (labelFun majorTickXs b)
          where
            f (x, l) = place dia p'
              where
                dia = view tickLabelTextFun tickLabelsD txtAlign l
                p' = p & ep e  .~ x
                       & ep eO .~ y
                       -- & logPoint ls
                       & coscale
                       & papply t
                       & ep eO +~ negate' (tickLabelsD ^. tickGap)

    -- grid
    grid = majorLines <> minorLines
      where
        majorLines = foldMap mkGridLine majorGridXs
                       # tStroke
                       # applyStyle (gridD ^. majorGridStyle)
        majorGridXs = (gridD ^. majorGridF) majorTickXs' b
        --
        minorLines = foldMap mkGridLine minorGridXs
                       # tStroke
                       # applyStyle (gridD ^. minorGridStyle)
        minorGridXs = (gridD ^. minorGridF) minorTickXs' b
        -- --
        mkGridLine x = pathFromVertices [f y0, f y1]
          where f y = over lensP ((el e .~ x) . (el eO .~ y)) p
        --
        gridD = a ^. axisGridLines ^. el e -- :: GridLines N

    -- ticks
    ticks = foldMap drawTicks ys

    drawTicks (pos,y) = majorTicks <> minorTicks
      where
        majorTicks = foldMap (positionTick majorTick) majorTickXs'
                       # stroke
                       # applyStyle (ticksD ^. majorTickStyle)
        --
        minorTicks = foldMap (positionTick minorTick) minorTickXs'
                       # stroke
                       # applyStyle (ticksD ^. minorTickStyle)
        --
        minorTick = someTick (ticksD ^. minorTickAlign) (ticksD ^. minorTickLength)
        majorTick = someTick (ticksD ^. majorTickAlign) (ticksD ^. majorTickLength)
        --
        someTick tType d = pathFromVertices $
          case tType  of
            AutoTick ->
              case pos of
                LowerAxis  -> [origin & ep eO -~ d, origin]
                MiddleAxis -> [origin & ep eO -~ d, origin & ep eO +~ d]
                UpperAxis  -> [origin, origin & ep eO +~ d]
            TickSpec (fromRational -> aa) (fromRational -> bb) ->
              case pos of
                UpperAxis -> [origin & ep eO -~ d*bb, origin & ep eO +~ d*aa]
                _         -> [origin & ep eO -~ d*aa, origin & ep eO +~ d*bb]
            NoTick -> []
        -- middleTick d =
        --   pathFromVertices
        positionTick tick x = place tick p'
          where
            p' = over lensP ((el e .~ x) . (el eO .~ y)) p
                   # transform t

    -- axis lines
    line = foldMap mkline (map snd ys) -- merge with ticks?
             # transform t
             # stroke
             # applyStyle (a ^. axisLine e . axisArrowOpts . _Just . shaftStyle)
             # lineCap LineCapSquare
      where
        -- TODO: Arrow for R3
        mkline y = pathFromVertices
         $ map (\x -> over lensP ((el e .~ x) . (el eO .~ y)) p) [x0, x1] :: Path v n

    -- measurements
    b@(x0,x1)  = bs ^. el e :: (n, n)
    -- b@(x0,x1)  = over both ((*ssX) . logNumber (ls^.el e)) $ bs ^. el e :: (n, n)
    coscale = ep e %~ coscaleNum
    coscaleNum = scaleNum (bs ^. el e) (ls ^. el e)
    -- ssX = x10 / logNumber (ls^.el e) x10
    yb@(y0,y1) = bs ^. el eO . if lp == UpperLabels
                                 then swapped
                                 else id
    --
    ticksD       = a ^. axisTicks . el e
    majorTickXs  = (ticksD ^. majorTicksFun) b
    majorTickXs' = map coscaleNum majorTickXs
    minorTickXs  = (ticksD ^. minorTicksFun) majorTickXs b
    minorTickXs' = map coscaleNum minorTickXs
    -- majorTickXs = logNumber (ls ^. el e) <$> (ticksD ^. majorTicksFun) b
    -- minorTickXs = logNumber (ls ^. el e) <$> (ticksD ^. minorTicksFun) majorTickXs b
    --
    ys       = getAxisLinePos yb lineType
    lineType = a ^. axisLines . el e . axisLineType
    txtAlign = case lp of
                  LowerLabels -> BoxAlignedText 0.5 1
                  LeftLabels  -> BoxAlignedText 1   0.5
                  RightLabels -> BoxAlignedText 0   0.5
                  UpperLabels -> BoxAlignedText 1   0
                  _           -> error "No labels" -- XXX Temporary
    -- t2 = scaling 4
    --
    negate' = if lp == UpperLabels || lp == RightLabels
                then id
                else negate

primStroke :: (Ord n, Typeable n, Typeable v, Renderable (Path v n) b)
           => Path v n -> QDiagram b v n Any
primStroke path =
  mkQD (Prim path)
       mempty
       mempty
       mempty
       mempty

------------------------------------------------------------------------
-- Calculating the bounds and scales
------------------------------------------------------------------------

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

workOutScale :: (v ~ BaseSpace v, HasLinearMap v, V (v n) ~ v, Distributive v, OrderedField n, Applicative v, Metric v)
  => BoundingBox v n -> Axis b v n
  -> (BaseSpace v (n,n), Transformation (BaseSpace v) n, Transformation (BaseSpace v) n)
workOutScale bb a = (enlargedBounds, aspectScaling, specScaling)
 where
    enlargedBounds = workOutUsedBounds
                       aScaling
                       -- disgusting
                       (view lensP . uncurry (liftA2 (,)) <$> getCorners bb)
                       bnd

    -- the vector that points from the lower bound to the upper bound of the
    -- axis
    v  = uncurry (flip (-)) <$> enlargedBounds
    v' = apply aspectScaling v
    specScaling = requiredScaling spec v'
    aspectScaling
      -- if any of the aspect ratios are committed we use the aspect ratio from
      -- aScaling
      | anyOf (folded . aspectRatio) (is _Commit) aScaling
          = vectorScaling (view (aspectRatio . _recommend) <$> aScaling)
      -- otherwise all ratios are just recommend, ignore them and scale such
      -- that each axis is the same length
      | otherwise = inv $ vectorScaling v
    --
    spec     = a ^. axisSize
    aScaling = a ^. axisScaling
    -- bb       = a ^. axisPlots . folded . to boundingBox
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


getAxisLinePos :: (Num n, Ord n) => (n, n) -> AxisLineType -> [(AxisPos, n)]
getAxisLinePos (a,b) aType = case aType of
  BoxAxisLine    -> [(LowerAxis, a), (UpperAxis, b)]
  LeftAxisLine   -> [(LowerAxis, a)]
  MiddleAxisLine -> [(,) MiddleAxis $
                     if | a > 0     -> a
                        | b < 0     -> b
                        | otherwise -> 0]
  RightAxisLine  -> [(UpperAxis, b)]
  NoAxisLine     -> []

              -- MiddleAxisLabel -> (x0 + x1) / 2
              -- LowerAxisLabel  -> x0
              -- UpperAxisLabel  -> x1

data AxisPos = LowerAxis | MiddleAxis | UpperAxis

------------------------------------------------------------------------
-- Elements
------------------------------------------------------------------------

-- Ticks ---------------------------------------------------------------

-- renderTicks
--   :: (TypeableFloat n, HasLinearMap v, Metric v, Typeable v,
--       Renderable (Path v n) b, OrderedField n)
--   => Point v n -- start point
--   -- minor
--   -> [n]   -- positions
--   -> (n,n) -- (lower, upper)
--   -- major
--   -> [n]   -- positions
--   -> (n,n) -- (lower, upper)
--   -> E v   -- direction of axis
--   -> E v   -- orthogonal direction of axis
--   -> (Path v n, Path v n) -- resulting ticks
-- renderTicks p0 t b ticks e down eO = majorTicks <> minorTicks
--   where
--     majorTicks = foldMap (positionTick majorTick) majorTickXs
--                    # primStroke
--                    # applyStyle (ticks ^. majorTickStyle)
--     --
--     minorTicks = foldMap (positionTick minorTick) minorTickXs
--                    # primStroke
--                    # applyStyle (ticks ^. minorTickStyle)
--     --
--     minorTick = middleTick (ticks ^. minorTickLength)
--     majorTick = middleTick (ticks ^. majorTickLength)
--     --
--     drawTick  d =
--       fromVertices
--         [ origin & ep eO -~ d
--         , origin & ep eO +~ d ]
--         # whenever down reversing
--     middleTick d =
--       fromVertices
--         [ origin & ep eO -~ d
--         , origin & ep eO +~ d ]
--         # whenever down reversing

--     positionTick tick x = place tick p'
--       where p' = p0 & ep e .~ x & papply t

--     majorTickXs = (ticks ^. majorTicksFun) b
--     minorTickXs = (ticks ^. minorTicksFun) majorTickXs b

-- Gird ----------------------------------------------------------------

-- renderGrid
--   :: (TypeableFloat n, HasLinearMap v, Metric v, Typeable v,
--       Renderable (Path v n) b, OrderedField n)
--   => Point v n -- start point
--   -- minor
--   -> [n]   -- positions
--   -> (n,n) -- (lower, upper)
--   -- major
--   -> [n]   -- positions
--   -> (n,n) -- (lower, upper)
--   -> E v   -- direction of axis
--   -> E v   -- orthogonal direction of axis
--   -> (Path v n, Path v n) -- resulting ticks
-- renderTicks p0 t b ticks e down eO = majorTicks <> minorTicks

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

ep :: E v -> Lens' (Point v x) x
ep (E l) = lensP . l
{-# INLINE ep #-}


------------------------------------------------------------------------
-- Polar
------------------------------------------------------------------------

instance
     (Typeable b,
      Enum n,
      TypeableFloat n,
      Renderable (Path V2 n) b,
      Renderable (Text n) b) => RenderAxis b Polar n where
  renderAxis = renderPolarAxis

renderPolarAxis
  :: (Typeable b,
      Enum n,
      TypeableFloat n,
      Renderable (Path V2 n) b,
      Renderable (Text n) b)
  => Axis b Polar n -> QDiagram b V2 n Any
renderPolarAxis a = frame 15
               $ legend
              -- <> colourBar
              <> circles
              <> rAxis
              <> plots
  where
    spec = AxisSpec (pure (-10, 10)) mempty (pure LinearAxis) (a ^. axisColourMap)
    -- plots    = foldMap (uncurry $ renderPlotable (AxisSpec xs t (a^.axisScale))) plots'
    plots    = foldMap (uncurry $ renderPlotable spec) plots' # scale 6

    -- drawAxis = axisOnBasis origin xs a (a^.axisScale) t
    --
    rAxis = (rline <> rticks) # scale 6

    rline = origin ~~ (10 *^ unitX) # lwO 2
    rticks = foldMap moveTo (map (\x -> mkP2 x 0) [1..9]) tick # lwO 1
    tick = unit_Y ~~ unitY & scale 0.2
    --
    circles = foldMap circle [1,3..9] # lwO 1 # lc grey # scale 6 # opacity 0.5
    --
    -- (xs, tv, t') = workOutScale a
    -- t = tv <> t'
    --
    bb = fromCorners (p2 (-10,-10)) (p2 (10,10)) -- (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    legend = drawLegend bb (a ^. axisLegend) (toList plots')
    --
    -- pp = a ^. plotProperties
    -- preparePlots =
    --   zipWith (\theme p' -> modifyPlot (p' & properties . plotStyle .~ theme) pp)
    --           (a ^.. axisStyle . axisStyles)

    -- The colour bar
    -- cbo = a ^. axisColourBar
    --         & cbExtent .~ ex'
    -- ex' = orient (cbo ^. cbOrientation) (V2 (width bb) 15) (V2 15 (height bb))
    -- colourBar = undefined -- addColourBar bb cbo (pp ^. plotColourMap) 0 1

    -- Rendering plots
    -- preparePlots :: [ModifiedPlot b v n] -> [(Plot b v n, PlotProperties b v n)]
    preparePlots =
      zipWith (\theme p' -> modifyPlot p' (startingProperties theme))
              (a ^.. axisStyle . axisStyles)
    plots'     = a ^. axisPlots . to preparePlots

