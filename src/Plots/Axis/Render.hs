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
-- Copyright   :  (C) 2016 Christopher Chalmers
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

import           Data.Foldable
import           Data.Typeable
import           Data.Bool

import           Diagrams.BoundingBox
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Linear                     hiding (translation, rotate)

import           Diagrams.Coordinates.Polar

import           Plots.Axis
import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Line
import           Plots.Axis.Scale
import           Plots.Axis.Title
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Style
import           Plots.Types
import           Plots.Util

import           Prelude

-- | Build a list of styled plots from the axis, ready to be rendered.
--   This takes into account any 'AxisStyle' changes and applies the
--   'finalPlots' modifications.
buildPlots :: BaseSpace c ~ v => Axis b c n -> [StyledPlot b v n]
buildPlots a = map (appEndo $ a ^. plotModifier)
             $ zipWith styleDynamic (a ^.. axisStyles) (a ^. axisPlots)
             -- TODO: correct order

-- -- | Build the axis spec from the axis parameters. The AxisSpec contains
-- --   the size infomation needed to render the axis.
-- buildAxisSpec :: BaseSpace c ~ v => Axis b c n -> (AxisSpec v n, [StyledPlot])
-- buildAxisSpec a = AxisSpec
--   { _specBounds = xs
--   , _specTrans  = t
--   , _specScale  = (a^.axes . column logScale)
--   , _specColourMap = (a ^. axisColourMap)
--   }
--     (xs, tv, t') = calculateScaling (a^.axes.column axisScaling) (boundingBox styledPlots)


--     -- First we need to gather infomation from the axis, this is done by
--     -- building the styled plots (not yet rendered). The bounding box of
--     -- these styled plots is used along with the axis scaling infomation
--     -- from the axes to calculate the bounds and transforms needed to
--     -- build the rest of the axis.
--     styledPlots  = buildPlots a
--     (xs, tv, t') = calculateScaling (a^.axes.column axisScaling) (boundingBox styledPlots)
--     t            = tv <> t'

--     -- To render the plots we need to give the AxisSpec to the styled
--     -- plots.
--     spec          = AxisSpec xs t (a^.axes . column logScale) (a ^. axisColourMap)
--     renderedPlots = foldMap (renderStyledPlot spec) styledPlots

--     -- Now we draw each of the individual axes.
--     drawAxis ll ll2 = axisOnBasis origin xs (a^.axes.el ll) (a^.axes.column logScale) t ll ll2

--     -- Alls that's left are the legend, colour bar and title.  The
--     -- rendering gubbins of these are in their respective modules.
--     bb   = fromCorners (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
--     leg  = drawLegend bb (styledPlotLegends styledPlots) (a ^. legend)
--     cBar = addColourBar bb (a^.colourBar) (a ^. axisColourMap) (0,1)
--     -- ttl = drawTitle bb (a^.title)

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
--     legend = drawLegend bb (a ^. legend) (plots' ^.. traversed)
--                         -- (a ^.. axisPlots . traversed . genericPlot)
--     --
--     plots' = a ^. axisPlots . to applyTheme
--     -- TODO: fix this
--     applyTheme = zipWith (\axisEntry -> over plotThemeEntry (Commit . fromCommit axisEntry)) (a ^. axisTheme)
--     --
--     l = a ^. axisLinearMap

-- | The 'RenderAxis' class provides a default way to render an axis for
--  each space.
instance (TypeableFloat n, Renderable (Path V2 n) b)
    => RenderAxis b V2 n where
  -- | Render an axis and its plots, as well as the legend and colour
  --   bar.
  renderAxis = renderR2Axis

renderR2Axis :: (TypeableFloat n, Renderable (Path V2 n) b)
  => Axis b V2 n -> QDiagram b V2 n Any
renderR2Axis a = frame 40
               $ leg
              <> ttl
              <> cBar
              <> drawAxis ex ey LowerLabels
              <> drawAxis ey ex LeftLabels
              <> plots
  where
    spec  = AxisSpec xs t (a^.axes . column logScale) (a ^. axisColourMap)
    plots = foldMap (renderStyledPlot spec) styledPlots
    drawAxis ll ll2 = axisOnBasis origin xs (a^.axes.el ll) (a^.axes.column logScale) t ll ll2
    --
    (xs, tv, t') = calculateScaling (a^.axes.column axisScaling) (boundingBox styledPlots)
    t = tv <> t'
    --
    bb = fromCorners (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    leg = drawLegend bb (styledPlotLegends styledPlots) (a ^. legend)
    --

    -- The colour bar
    cBar = addColourBar bb (a^.colourBar) (a ^. axisColourMap) (a^.colourBarRange)
    -- title
    ttl = drawTitle bb (a^.title)
    --
    styledPlots = buildPlots a

-- | The position of axis labels for a
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
  -> SingleAxis b v n -- axis data
  -> v LogScale       -- log scale
  -> T2 n             -- transformation to apply to positions of things
  -> E v              -- direction of axis
  -> E v              -- orthogonal direction of axis
  -> LabelPosition    -- where (if at all) should labels be placed?
  -> QDiagram b V2 n Any   -- resulting axis
axisOnBasis p bs a ls t e eO lp
  | a ^. hidden = phantom axis
  | otherwise   = axis
  where
    axis = tickLabels <> axLabels <> ticks <> line <> grid
    tStroke = stroke . transform t

    -- axis labels (x,y etc.)
    axLabels
      | null txt || lp == NoLabels || a ^. axisLabel . hidden
                  = mempty
      | otherwise = (a ^. axisLabelTextFunction) txtAlign txt
                      # moveTo p'
                      # applyStyle (a ^. axisLabelStyle)
      where
        p' = p & ep e  .~ x
               & ep eO .~ y0
               -- & logPoint ls
               & coscale
               & papply t
               & ep eO +~ negate' labelGap
        labelGap = a ^. axisLabelGap
        txt      = a ^. axisLabelText
        x = case a ^. axisLabelPosition of
              MiddleAxisLabel -> (x0 + x1) / 2
              LowerAxisLabel  -> x0
              UpperAxisLabel  -> x1
        -- axLabelD = a ^. axisLabels . el e

    -- tick labels
    tickLabels
      | lp == NoLabels || a ^. tickLabel . hidden = mempty
      | otherwise = foldMap drawLabels (map snd $ take 1 ys)
                      # applyStyle (a ^. tickLabelStyle)
      where
        -- tickLabelsD  = a ^. axisTickLabels . el e
        labelFun     = a ^. tickLabelFunction
        drawLabels y = foldMap f (labelFun majorTickXs b)
          where
            f (x, l) = place dia p'
              where
                dia = view tickLabelTextFunction a txtAlign l
                p' = p & ep e  .~ x
                       & ep eO .~ y
                       -- & logPoint ls
                       & coscale
                       & papply t
                       & ep eO +~ negate' (a ^. tickLabelGap)

    -- the grid

    grid = majorLines <> minorLines
      where
        majorLines
          | a ^. majorGridLines . hidden = mempty
          | otherwise = foldMap mkGridLine majorGridXs
                          # tStroke
                          # applyStyle (a ^. majorGridLinesStyle)
        majorGridXs = view majorGridLinesFunction a majorTickXs' b
        --
        minorLines
          | a ^. minorGridLines . hidden = mempty
          | otherwise = foldMap mkGridLine minorGridXs
                       # tStroke
                       # applyStyle (a ^. minorGridLinesStyle)
        minorGridXs = view minorGridLinesFunction a minorTickXs' b
        -- --
        mkGridLine x = pathFromVertices [f y0, f y1]
          where f y = over lensP ((el e .~ x) . (el eO .~ y)) p
        --
        -- gridD = a ^. axisGridLines ^. el e -- :: GridLines N

    -- the ticks

    ticks = foldMap drawTicks ys

    drawTicks (pos,y) = maTicks <> miTicks
      where
        maTicks
          | a ^. majorTicks . hidden = mempty
          | otherwise = foldMap (positionTick majorTick) majorTickXs'
                       # stroke
                       # applyStyle (a ^. majorTicksStyle)
        --
        miTicks
          | a ^. minorTicks . hidden = mempty
          | otherwise = foldMap (positionTick minorTick) minorTickXs'
                       # stroke
                       # applyStyle (a ^. minorTicksStyle)
        --
        minorTick = someTick (a ^. minorTicksAlignment) (a ^. minorTicksLength)
        majorTick = someTick (a ^. majorTicksAlignment) (a ^. majorTicksLength)
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
            -- NoTick -> []
        -- middleTick d =
        --   pathFromVertices
        positionTick tick x = place tick p'
          where
            p' = over lensP ((el e .~ x) . (el eO .~ y)) p
                   # transform t

    -- axis lines

    line
      | a ^. axisLine . hidden = mempty
      | otherwise = foldMap mkline (map snd ys) -- merge with ticks?
             # transform t
             # stroke
             -- # applyStyle (a ^. axisLine e . axisArrowOpts . _Just . shaftStyle)
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
    -- ticksD       = a ^. axisTicks . el e
    majorTickXs  = view majorTicksFunction a b
    majorTickXs' = map coscaleNum majorTickXs
    minorTickXs  = view minorTicksFunction a majorTickXs b
    minorTickXs' = map coscaleNum minorTickXs
    -- majorTickXs = logNumber (ls ^. el e) <$> (ticksD ^. majorTicksFun) b
    -- minorTickXs = logNumber (ls ^. el e) <$> (ticksD ^. minorTicksFun) majorTickXs b
    --
    ys       = getAxisLinePos yb lineType
    lineType = a ^. axisLineType
    txtAlign =
      case lp of
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

-- | Stroke without any envelope, trace, query etc.
primStroke :: (Ord n, Typeable n, Typeable v, Renderable (Path v n) b)
           => Path v n -> QDiagram b v n Any
primStroke path =
  mkQD (Prim path)
       mempty
       mempty
       mempty
       mempty

-- utilities

translationE :: (Num n, HasLinearMap v) => E v -> n -> Transformation v n
translationE (E l) x = translation (zero & l .~ x)

scaleE :: (Additive v, Fractional n) => E v -> n -> Transformation v n
scaleE e s = fromLinear f f
  where f = (el e *~ s) <-> (el e //~ s)

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

instance (TypeableFloat n, Renderable (Path V2 n) b)
    => RenderAxis b Polar n where
  renderAxis = renderPolarAxis

renderPolarAxis
  :: (TypeableFloat n, Renderable (Path V2 n) b)
  => Axis b Polar n -> QDiagram b V2 n Any
renderPolarAxis a = frame 15
               $ leg
              -- <> colourBar
              -- <> circles
              <> rAxis
              <> plots
  where
    spec = AxisSpec (pure (-20, 20)) mempty (pure LinearAxis) (a ^. axisColourMap)
    plots    = foldMap (renderStyledPlot spec) styledPlots # scale 6

    -- drawAxis = axisOnBasis origin xs a (a^.axisScale) t
    --
    -- rAxis = (rline <> rticks) # scale 6
    rAxis = drawPolarAxis spec' (a ^. axes)
    spec' = AxisSpec (pure (-20, 20)) mempty (pure LinearAxis) (a ^. axisColourMap)

    -- rline = origin ~~ (10 *^ unitX) # lwO 2
    -- rticks = foldMap moveTo (map (\x -> mkP2 x 0) [1..9]) tick # lwO 1
    -- tick = unit_Y ~~ unitY & scale 0.2
    --
    -- circles = foldMap circle [1,3..9] # lwO 1 # lc grey # scale 6 # opacity 0.5
    --
    -- (xs, tv, t') = workOutScale a
    -- t = tv <> t'
    --
    bb = fromCorners (p2 (-20,-20)) (p2 (20,20)) -- (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    leg = drawLegend bb (styledPlotLegends styledPlots) (a ^. legend)
    --

    styledPlots = map (appEndo $ a ^. plotModifier)
                $ zipWith styleDynamic (a ^.. axisStyles) (a ^. axisPlots)


    -- -- First we need to gather infomation from the axis, this is done by
    -- -- building the styled plots (not yet rendered). The bounding box of
    -- -- these styled plots is used along with the axis scaling infomation
    -- -- from the axes to calculate the bounds and transforms needed to
    -- -- build the rest of the axis.
    -- styledPlots  = buildPlots a
    -- (xs, tv, t') = calculateScaling (a^.axes.column axisScaling) (boundingBox styledPlots)
    -- t            = tv <> t'

    -- -- To render the plots we need to give the AxisSpec to the styled
    -- -- plots.
    -- spec          = AxisSpec xs t (a^.axes . column logScale) (a ^. axisColourMap)
    -- renderedPlots = foldMap (renderStyledPlot spec) styledPlots

    -- -- Now we draw each of the individual axes.
    -- drawAxis ll ll2 = axisOnBasis origin xs (a^.axes.el ll) (a^.axes.column logScale) t ll ll2

    -- -- Alls that's left are the legend, colour bar and title.  The
    -- -- rendering gubbins of these are in their respective modules.
    -- bb   = fromCorners (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    -- leg  = drawLegend bb (styledPlotLegends styledPlots) (a ^. legend)
    -- cBar = addColourBar bb (a^.colourBar) (a ^. axisColourMap) (0,1)
    -- -- ttl = drawTitle bb (a^.title)

drawPolarAxis :: forall b n. (Renderable (Path V2 n) b, TypeableFloat n)
              => AxisSpec V2 n -> Polar (SingleAxis b V2 n) -> QDiagram b V2 n Any
drawPolarAxis spec (Polar (V2 rA thetaA)) = rAx <> thetaAx where

  r = 100

  ----------------------------------------------------------------------
  -- Radial axis
  ----------------------------------------------------------------------

  -- The radial axis consists of an axis line from the centre to the
  -- edge. The ticks and tickLabels are along this line. The grid lines
  -- are made up of circles that pass through the line, centered at the
  -- center of the plot.
  rAx
    | rA ^. hidden = mempty
    | otherwise = rAxLine <> rAxLabel <> rAxTicks <> rAxTickLabels <> rAxGridLines

  rAxLine = line # whenever (rA ^. axisLine . hidden) phantom
    where
      -- XXX for now the radial axis is on the theta=0 line. Need some
      -- way to change this
      line = (origin ~~ mkP2 r 0) # applyStyle (rA^.axisLineStyle)

  -- Radial axis label -------------------------------------------------

  rAxLabel
    | null rTxt || rA ^. axisLabel . hidden = mempty
    | otherwise = view axisLabelTextFunction rA rLabelAlign rTxt
                    # translate rLabelPos
                    # applyStyle (rA ^. axisLabelStyle)

  rLabelPos = V2 x (- view axisLabelGap rA) where
    x = case rA ^. axisLabelPosition of
          MiddleAxisLabel -> r/2
          LowerAxisLabel  -> 0
          UpperAxisLabel  -> r
  rTxt = rA ^. axisLabelText
  rLabelAlign = BaselineText

  -- Radial axis ticks -------------------------------------------------

  -- The positions of major and minor ticks along the radial axis
  majorTickRs = view majorTicksFunction rA (0,r)
  minorTickRs = view minorTicksFunction rA majorTickRs (0,r)

  -- Major and minor ticks are placed along the line at the calculated
  -- positions majorTickRs
  rAxTicks      = rAxMajorTicks <> rAxMinorTicks
  rAxMajorTicks
    | rA ^. majorTicks . hidden = mempty
    | otherwise = foldMap (\x -> rAxMajorTick # translateX x) majorTickRs
  rAxMinorTicks
    | rA ^. minorTicks . hidden = mempty
    | otherwise = foldMap (\x -> rAxMinorTick # translateX x) minorTickRs

  -- The paths used for individual major and minor ticks
  rAxMajorTick = someTick (rA ^. majorTicksAlignment) (rA ^. majorTicksLength)
  rAxMinorTick = someTick (rA ^. minorTicksAlignment) (rA ^. minorTicksLength)

  someTick tType d = case tType of
    TickSpec (fromRational -> aa) (fromRational -> bb)
             -> mkP2 0 (-d*bb) ~~ mkP2 0 (d*aa)
    AutoTick -> mkP2 0 (-d)    ~~ mkP2 0 d

  -- Radial grid2lines -------------------------------------------------

  rAxGridLines
    -- - | rA ^. gridLines . hidden = mempty
    | otherwise                = rMajorGridLines <> rMinorGridLines

  majorGridRs :: [n]
  majorGridRs    = view majorGridLinesFunction rA majorTickRs (0,r)

  rMajorGridLines :: QDiagram b V2 n Any
  rMajorGridLines
    | rA ^. majorGridLines . hidden = mempty
    | otherwise  = foldMap circle (filter (>0) majorGridRs)
                     # applyStyle (rA ^. majorGridLinesStyle)

  minorGridRs :: [n]
  minorGridRs    = view minorGridLinesFunction rA minorTickRs (0,r)
  rMinorGridLines :: QDiagram b V2 n Any
  rMinorGridLines
    | rA ^. minorGridLines . hidden = mempty
    | otherwise = foldMap circle (filter (>0) minorGridRs)
                    # applyStyle (rA ^. minorGridLinesStyle)

  -- Radial tick labels ------------------------------------------------

  rAxTickLabels :: QDiagram b V2 n Any
  rAxTickLabels
    | rA ^. tickLabel . hidden = mempty
    | otherwise                = foldMap rDrawTickLabel tickLabelRs

  -- The positions of the tick labels.
  tickLabelRs :: [(n, String)]
  tickLabelRs = view tickLabelFunction rA majorTickRs (0,r)

  -- Draw a single tick label given the position and the string to use
  rDrawTickLabel :: (n,String) -> QDiagram b V2 n Any
  rDrawTickLabel (x,label) =
    view tickLabelTextFunction rA (BoxAlignedText 0.5 1) label
      # translate (V2 x (- view axisLabelGap rA))
      # applyStyle (rA ^. tickLabelStyle)

  ----------------------------------------------------------------------
  -- Angular axis
  ----------------------------------------------------------------------

  -- The angular axis is a circular line around the perimieter of the
  -- polar plot. The Ticks and tick labels are placed around this
  -- perimeter. The Grid lines go from the perimeter to the center.
  thetaAx
    | thetaA ^. hidden = mempty
    | otherwise = thetaAxLine <> thetaAxLabel
         <> thetaAxTicks <> thetaAxTickLabels <> thetaAxGridLines

  theta = 2*pi
  thetaAxLine = line # whenever (thetaA ^. axisLine . hidden) phantom
    where
      -- XXX for now the radial axis is on the theta=0 line. Need some
      -- way to change this
      line = (origin ~~ mkP2 r 0) # applyStyle (thetaA^.axisLineStyle)

  -- Angular axis label ------------------------------------------------

  -- Where should the label go?
  thetaAxLabel
    | null thetaTxt || thetaA ^. axisLabel . hidden = mempty
    | otherwise = view axisLabelTextFunction thetaA thetaLabelAlign thetaTxt
                    # translate thetaLabelPos
                    # applyStyle (thetaA ^. axisLabelStyle)

  thetaLabelPos = V2 x (- view axisLabelGap thetaA) where
    x = case thetaA ^. axisLabelPosition of
          MiddleAxisLabel -> theta/2
          LowerAxisLabel  -> 0
          UpperAxisLabel  -> theta
  thetaTxt = thetaA ^. axisLabelText
  thetaLabelAlign = BaselineText

  -- Angular axis ticks ------------------------------------------------

  -- The positions of major and minor ticks along the angular axis
  majorTickThetas = view majorTicksFunction thetaA (0,theta)
  minorTickThetas = view minorTicksFunction thetaA majorTickThetas (0,theta)

  -- Major and minor ticks are placed along perimeter, facing the center
  -- of the axis. Ticks start of horizonal and are rotated to the
  -- correct position on the axis.
  thetaAxTicks      = thetaAxMajorTicks <> thetaAxMinorTicks
  thetaAxMajorTicks
    | thetaA ^. majorTicks . hidden = mempty
    | otherwise = foldMap (\phi -> thetaAxMajorTick # translateX r # rotate (phi@@rad)) majorTickThetas
  thetaAxMinorTicks
    | thetaA ^. minorTicks . hidden = mempty
    | otherwise = foldMap (\phi -> thetaAxMinorTick # translateX r # rotate (phi@@rad)) minorTickThetas

  -- The paths used for individual major and minor ticks
  thetaAxMajorTick = someThetaTick (thetaA ^. majorTicksAlignment) (thetaA ^. majorTicksLength)
  thetaAxMinorTick = someThetaTick (thetaA ^. minorTicksAlignment) (thetaA ^. minorTicksLength)

  someThetaTick tType d = case tType of
    TickSpec (fromRational -> aa) (fromRational -> bb)
             -> mkP2 (-d*bb) 0 ~~ mkP2 (d*aa) 0
    AutoTick -> mkP2 (-d) 0    ~~ mkP2 d 0

  -- Angular grid lines ------------------------------------------------

  -- grid lines go from the centre of the axis to the perimeter
  thetaAxGridLines
    -- - | thetaA ^. girdLines . hidden = mempty
    | otherwise                = thetaMajorGridLines <> thetaMinorGridLines

  majorGridThetas :: [n]
  majorGridThetas = view majorGridLinesFunction thetaA majorTickThetas (0,theta)

  thetaMajorGridLines :: QDiagram b V2 n Any
  thetaMajorGridLines
    | thetaA ^. majorGridLines . hidden = mempty
    | otherwise  = foldMap (\phi -> origin ~~ mkP2 r 0 # rotate (phi@@rad)) majorGridThetas
                     # applyStyle (thetaA ^. majorGridLinesStyle)

  minorGridThetas :: [n]
  minorGridThetas    = view minorGridLinesFunction thetaA minorTickThetas (0,theta)
  thetaMinorGridLines :: QDiagram b V2 n Any
  thetaMinorGridLines
    | thetaA ^. minorGridLines . hidden = mempty
    | otherwise = foldMap (\phi -> origin ~~ mkP2 r 0 # rotate (phi@@rad)) minorGridThetas
                    # applyStyle (thetaA ^. minorGridLinesStyle)

  -- Angular tick labels -----------------------------------------------

  thetaAxTickLabels :: QDiagram b V2 n Any
  thetaAxTickLabels
    | thetaA ^. tickLabel . hidden = mempty
    | otherwise                = foldMap thetaDrawTickLabel tickLabelThetas

  -- The positions of the tick labels.
  tickLabelThetas :: [(n, String)]
  tickLabelThetas = view tickLabelFunction thetaA majorTickThetas (0,theta)

  -- Draw a single tick label given the position and the string to use
  thetaDrawTickLabel :: (n,String) -> QDiagram b V2 n Any
  thetaDrawTickLabel (x,label) =
    view tickLabelTextFunction thetaA a label
      # translate v
      # applyStyle (thetaA ^. tickLabelStyle)
        where v = mkPolar (r + view axisLabelGap thetaA) (x@@rad) ^. xy_
              -- a = BoxAlignedText (0.5-cos x/2) (0.5-sin x/2)
              a = BoxAlignedText 0.5 0.5

    -- line
    --   | a ^. axisLine . hidden = mempty
    --   | otherwise = foldMap mkline (map snd ys) -- merge with ticks?
    --          # transform t
    --          # stroke
    --          -- # applyStyle (a ^. axisLine e . axisArrowOpts . _Just . shaftStyle)
    --          # lineCap LineCapSquare
    --   where
    --     -- TODO: Arrow for R3
    --     mkline y = pathFromVertices
    --      $ map (\x -> over lensP ((el e .~ x) . (el eO .~ y)) p) [x0, x1] :: Path v n

-- | Apply a function if the predicate is true.
whenever :: Bool -> (a -> a) -> a -> a
whenever b f = bool id f b
