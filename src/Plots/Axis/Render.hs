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

-- Orphan Mainable Axis instance.
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
module Plots.Axis.Render
  ( -- * Rendering axes
    RenderAxis (..)
  , r2AxisMain

    -- * Low level
  , buildPlots
  )where

import           Data.Bool
import           Data.Foldable              as F
import           Data.List                  (sort)
import           Data.Typeable

import           Geometry.BoundingBox
import           Diagrams.Prelude
import           Geometry.Envelope
import           Diagrams.TwoD.Text
import           Linear                     hiding (rotate, translation)

import           Diagrams.Backend.CmdLine
import           Diagrams.Coordinates.Polar

import           Plots.Axis
import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Line
import           Plots.Axis.Scale
import           Plots.Axis.Ticks
import           Plots.Axis.Title
import           Plots.Legend
import           Plots.Style
import           Plots.Types
import           Plots.Util

import           Prelude

import qualified Numeric.Interval.NonEmpty as I

import Geometry.TwoD.Transform
import Geometry.TwoD.Ellipse

import Diagrams.Types (Prim (..), mkQD)

------------------------------------------------------------------------
-- Mainable instances
------------------------------------------------------------------------

instance WithOutcome (Axis Polar)
instance WithOutcome (Axis V2)
instance WithOutcome (Axis V3)

instance RenderOutcome t (Diagram V2) => RenderOutcome t (Axis Polar) where
  type MainOpts t (Axis Polar) = MainOpts t (Diagram V2)
  resultParser t _ = resultParser t (Proxy :: Proxy (Diagram V2))
  renderOutcome t opts axis = renderOutcome t opts (renderPolarAxis axis)

instance RenderOutcome t (Diagram V2) => RenderOutcome t (Axis V2) where
  type MainOpts t (Axis V2) = MainOpts t (Diagram V2)
  resultParser t _ = resultParser t (Proxy :: Proxy (Diagram V2))
  renderOutcome t opts axis = renderOutcome t opts (renderAxis axis)

instance RenderOutcome t (Diagram V3) => RenderOutcome t (Axis V3) where
  type MainOpts t (Axis V3) = MainOpts t (Diagram V3)
  resultParser t _ = resultParser t (Proxy :: Proxy (Diagram V3))
  renderOutcome t opts axis = renderOutcome t opts (renderAxis axis)


strokePathV :: (Typeable v, Metric v, Typeable n, OrderedField n) => Path v n -> QDiagram v n Any
strokePathV path = mkQD (Prim path) (getEnvelope path) mempty mempty

-- instance (TypeableFloat n, Mainable (Diagram V2))
--        => Mainable (Axis b Polar n) where
--   type MainOpts (Axis b Polar n) = MainOpts (Diagram V2)

--   mainRender opts = mainRender opts . renderAxis

-- instance (TypeableFloat n,
--           Renderable (Path V2 n) b,
--           Mainable (Diagram V2))
--        => Mainable (Axis b V2 n) where
--   type MainOpts (Axis b V2 n) = MainOpts (Diagram V2)

--   mainRender opts = mainRender opts . renderAxis

-- instance ToResult (Axis b v n) where
--   type Args (Axis b v n) = ()
--   type ResultOf (Axis b v n) = Axis b v n

--   toResult d _ = d

-- | 'mainWith' specialised to a 2D Axis.
r2AxisMain
  :: RenderOutcome t (Diagram V2)
  => t
  -> Axis V2
  -> IO ()
r2AxisMain = mainWith

------------------------------------------------------------------------
-- Low level functions
------------------------------------------------------------------------

-- | Build a list of styled plots from the axis, ready to be rendered.
--   This takes into account any 'AxisStyle' changes and applies the
--   'finalPlots' modifications.
--
--   The 'StyledPlots' can be rendered with 'renderStyledPlot' and the
--   legend entries can be obtained with 'styledPlotLegends'. This is
--   what 'renderAxis' can uses internally but might be useful for
--   debugging or generating your own legend.
buildPlots :: BaseSpace c ~ v => Axis c -> [StyledPlot v]
buildPlots a = map (appEndo $ a ^. plotModifier)
             $ zipWith styleDynamic (a ^.. axisStyles) (a ^. axisPlots)
             -- TODO: correct order

------------------------------------------------------------------------
-- Render axis
------------------------------------------------------------------------

-- | Renderable axes.
class RenderAxis c where
  -- | Render an axis to a diagram. The size of the diagram is
  --   determined by the 'axisSize'.
  renderAxis :: Axis c -> Diagram (BaseSpace c)

-- R2 rendering --------------------------------------------------------

-- | The 'RenderAxis' class provides a default way to render an axis for
--  each space.
instance RenderAxis V2 where
  -- | Render an axis and its plots, as well as the legend and colour
  --   bar.
  renderAxis = renderR2Axis

renderR2Axis :: Axis V2 -> Diagram V2
renderR2Axis a = frame 40
               $ leg
              <> ttl
              <> cBar
              <> plots
              <> drawAxis ex ey LowerLabels
              <> drawAxis ey ex LeftLabels
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

-- R3 rendering --------------------------------------------------------


instance RenderAxis V3 where
  renderAxis = renderR3Axis

renderR3Axis :: Axis V3 -> Diagram V3
renderR3Axis a = -- frame 15
               -- $ legend
               (plots :: Diagram V3)
              <> (drawAxis ex ey LowerLabels :: Diagram V3)
              <> (drawAxis ey ex UpperLabels :: Diagram V3)
              <> (drawAxis ez ey LowerLabels :: Diagram V3)
              <> (drawAxis ey ez NoLabels :: Diagram V3)
              <> (drawAxis ez ex NoLabels :: Diagram V3)
              <> (drawAxis ex ez NoLabels :: Diagram V3)
  where
    spec  = AxisSpec xs t (a^.axes . column logScale) (a ^. axisColourMap)
    plots = foldMap (renderStyledPlot spec) styledPlots
    drawAxis ll ll2 = axisOnBasis origin xs (a^.axes.el ll) (a^.axes.column logScale) t ll ll2
    --
    (xs, tv, t') = calculateScaling (a^.axes.column axisScaling) (boundingBox styledPlots)
    t = tv <> t'
    --
    -- bb = fromCorners (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    -- leg = drawLegend bb (styledPlotLegends styledPlots) (a ^. legend)
    --

    -- The colour bar
    -- cBar = addColourBar bb (a^.colourBar) (a ^. axisColourMap) (a^.colourBarRange)
    -- title
    -- ttl = drawTitle bb (a^.title)
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
  :: forall v. (HasLinearMap v, Typeable v)
  => Point v Double        -- start of axis
  -> v (Double, Double)         -- calculated bounds
  -> SingleAxis v  -- axis data
  -> v LogScale       -- log scale
  -> Transformation v Double        -- transformation to apply to positions of things
  -> E v              -- direction of axis
  -> E v              -- orthogonal direction of axis
  -> LabelPosition    -- where (if at all) should labels be placed?
  -> Diagram v   -- resulting axis
axisOnBasis p bs a ls t e eO lp
  | a ^. hidden = phantom axis
  | otherwise   = axis
  where
    axis = tickLabels <> axLabels <> ticks <> line <> grid
    tStroke = strokePathV . transform t

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
        drawLabels y = foldMap f (labelFun (filter inRange majorTickXs) b)
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
          | otherwise = foldMap mkGridLine majorGridXs'
                          # tStroke
                          # applyStyle (a ^. majorGridLinesStyle)
        majorGridXs  = view majorGridLinesFunction a majorTickXs b
        majorGridXs' = map coscaleNum (filter inRange majorGridXs)
        --
        minorLines
          | a ^. minorGridLines . hidden = mempty
          | otherwise = foldMap mkGridLine minorGridXs'
                       # tStroke
                       # applyStyle (a ^. minorGridLinesStyle)
        minorGridXs  = view minorGridLinesFunction a minorTickXs b
        minorGridXs' = map coscaleNum (filter inRange minorGridXs)
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
                       # strokePathV
                       # applyStyle (a ^. majorTicksStyle)
        --
        miTicks
          | a ^. minorTicks . hidden = mempty
          | otherwise = foldMap (positionTick minorTick) minorTickXs'
                       # strokePathV
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
             # strokePathV
             # lineCap LineCapSquare
             # applyStyle (a^.axisLineStyle)
      where
        -- TODO: Arrow for R3
        mkline y = pathFromVertices
         $ map (\x -> over lensP ((el e .~ x) . (el eO .~ y)) p) [x0, x1] :: Path v Double

    -- measurements
    b@(x0,x1)  = bs ^. el e :: (Double, Double) -- bounds
    coscale = ep e %~ coscaleNum
    coscaleNum = scaleNum (bs ^. el e) (ls ^. el e)
    yb@(y0,y1) = bs ^. el eO . if lp == UpperLabels
                                 then swapped
                                 else id
    inRange x = x >= x0 && x <= x1
    --
    majorTickXs  = sort $ view majorTicksFunction a b
    majorTickXs' = map coscaleNum (filter inRange majorTickXs)
    minorTickXs  = sort $ view minorTicksFunction a majorTickXs b
    minorTickXs' = map coscaleNum (filter inRange minorTickXs)
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

-- utilities

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
-- Utilities
------------------------------------------------------------------------

ep :: E v -> Lens' (Point v x) x
ep (E l) = lensP . l
{-# INLINE ep #-}

------------------------------------------------------------------------
-- Polar
------------------------------------------------------------------------

instance RenderAxis Polar where
  renderAxis = renderPolarAxis

-- | An lower and upper bound for the bounding radius using @n@ envelope
--   calculations. The more calculations used, the smaller the range of
--   the bound.
boundingRadiusR :: (InSpace V2 n a, Enveloped a) => Int -> a -> (n, n)
boundingRadiusR (max 3 -> n) e =
  case getEnvelope e of
    EmptyEnvelope -> (0,0)
    Envelope f  ->
      let thetas = map (@@rad) $ enumFromToN 0 tau n
          vs     = map angleDir thetas

          -- The lower bound is the maximum distance obtained from the
          -- envelope trials. We know the radius will be at least this far.
          lowerBound = F.foldr (\v r -> max (I.sup $ f v) r) 0 vs

          -- In the worst case, there will be a point at the intersection of
          -- two neighbouring bounding planes from the envelope calculations.
          -- We can calculate the distance to this intersecion using simple
          -- trigonometry.
          -- (Note, this is why we need at least three envelope calculations,
          -- otherwise there wouldn't be any intersection between the bounding
          -- planes)
          upperBound = lowerBound / cos (pi / fromIntegral n)

      in  (lowerBound, upperBound)

renderPolarAxis :: Axis Polar -> Diagram V2
renderPolarAxis a = frame 15
               $ leg
              -- <> colourBar
              -- <> circles
              <> plots
              <> theAxis
  where
    r = snd $ boundingRadiusR 30 styledPlots
    spec  = AxisSpec xs t (pure LinearAxis) (a ^. axisColourMap)
    plots = F.foldMap (renderStyledPlot spec) styledPlots

    dataBB = fromCorners (mkP2 (-r) (-r)) (mkP2 r r)
    (xs, tv, t') = calculateScaling (view _Wrapped $ a^.axes.column axisScaling) dataBB
    t = tv <> t'
    --
    theAxis = drawPolarAxis spec (a ^. axes)
    --
    bb = fromCorners (P . apply t $ fmap fst xs) (P . apply t $ fmap snd xs)
    leg = drawLegend bb (styledPlotLegends styledPlots) (a ^. legend)
    --
    styledPlots = map (appEndo $ a ^. plotModifier)
                $ zipWith styleDynamic (a ^.. axisStyles) (a ^. axisPlots)

drawPolarAxis :: AxisSpec V2 -> Polar (SingleAxis V2) -> Diagram V2
drawPolarAxis spec (Polar (V2 rA thetaA)) = fcA transparent $ rAx <> thetaAx where

  -- use a radius of the upper x bound for the axis (this is not ideal)
  r = spec ^. specBounds . _x . _2
  t = spec ^. specTrans
  s = avgScale t

  rInRange x = x >= 0 && x <= r*1.000001
  thetaInRange x = x >= 0 && x < tau

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
      line = fromVertices [origin, mkP2 r 0]
               # applyStyle (rA^.axisLineStyle)
               # transform t

  -- Radial axis label -------------------------------------------------

  rAxLabel
    | null rTxt || rA ^. axisLabel . hidden = mempty
    | otherwise = view axisLabelTextFunction rA rLabelAlign rTxt
                    # translate rLabelPos
                    # applyStyle (rA ^. axisLabelStyle)
                    # fc black

  rLabelPos = V2 (s*x) (- view axisLabelGap rA) where
    x = case rA ^. axisLabelPosition of
          MiddleAxisLabel -> r/2
          LowerAxisLabel  -> 0
          UpperAxisLabel  -> r
  rTxt = rA ^. axisLabelText
  rLabelAlign = BaselineText

  -- Radial ticks ------------------------------------------------------

  -- The positions of major and minor ticks along the radial axis
  majorTickRs  = view majorTicksFunction rA (0,r)
  majorTickRs' = map (*s) $ filter rInRange majorTickRs
  minorTickRs  = view minorTicksFunction rA majorTickRs (0,r)
  minorTickRs' = map (*s) $ filter rInRange minorTickRs

  -- Major and minor ticks are placed along the line at the calculated
  -- positions majorTickRs
  rAxTicks      = rAxMajorTicks <> rAxMinorTicks
  rAxMajorTicks
    | rA ^. majorTicks . hidden = mempty
    | otherwise = F.foldMap (\x -> rAxMajorTick # translateX x) majorTickRs'
                       # applyStyle (rA ^. majorTicksStyle)
  rAxMinorTicks
    | rA ^. minorTicks . hidden = mempty
    | otherwise = F.foldMap (\x -> rAxMinorTick # translateX x) minorTickRs'
                       # applyStyle (rA ^. minorTicksStyle)

  -- The paths used for individual major and minor ticks
  rAxMajorTick = someTick (rA ^. majorTicksAlignment) (rA ^. majorTicksLength)
  rAxMinorTick = someTick (rA ^. minorTicksAlignment) (rA ^. minorTicksLength)

  someTick tType d = case tType of
    TickSpec (fromRational -> aa) (fromRational -> bb)
             -> fromVertices [mkP2 0 (-d*bb), mkP2 0 (d*aa)]
    AutoTick -> fromVertices [mkP2 0 (-d)   , mkP2 0 d     ]

  -- Radial grid lines -------------------------------------------------

  rAxGridLines
    -- - | rA ^. gridLines . hidden = mempty
    | otherwise                = rMajorGridLines <> rMinorGridLines

  majorGridRs  = view majorGridLinesFunction rA majorTickRs (0,r)
  majorGridRs' = map (*s) $ filter rInRange majorGridRs

  rMajorGridLines :: Diagram V2
  rMajorGridLines
    | rA ^. majorGridLines . hidden = mempty
    | otherwise = F.foldMap circle (filter (>0) majorGridRs')
                    # applyStyle (rA ^. majorGridLinesStyle)

  minorGridRs  = view minorGridLinesFunction rA minorTickRs (0,r)
  minorGridRs' = map (*s) $ filter rInRange minorGridRs
  rMinorGridLines :: Diagram V2
  rMinorGridLines
    | rA ^. minorGridLines . hidden = mempty
    | otherwise = F.foldMap circle (filter (>0) minorGridRs')
                    # applyStyle (rA ^. minorGridLinesStyle)

  -- Radial tick labels ------------------------------------------------

  rAxTickLabels :: Diagram V2
  rAxTickLabels
    | rA ^. tickLabel . hidden = mempty
    | otherwise                = F.foldMap rDrawTickLabel tickLabelRs

  -- The positions of the tick labels.
  tickLabelRs :: [(Double, String)]
  tickLabelRs = view tickLabelFunction rA (filter rInRange majorTickRs) (0,r)

  -- Draw a single tick label given the position and the string to use
  rDrawTickLabel :: (Double,String) -> Diagram V2
  rDrawTickLabel (x,label) =
    view tickLabelTextFunction rA (BoxAlignedText 0.5 1) label
      # translate (V2 (s*x) (- view axisLabelGap rA))
      # applyStyle (rA ^. tickLabelStyle)
      # fc black

  ----------------------------------------------------------------------
  -- Angular axis
  ----------------------------------------------------------------------

  -- The angular axis is a circular line around the perimeter of the
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
      line = circle (s*r) # applyStyle (thetaA^.axisLineStyle)

  -- Angular axis label ------------------------------------------------

  -- Where should the label go?
  thetaAxLabel
    | null thetaTxt || thetaA ^. axisLabel . hidden = mempty
    | otherwise = view axisLabelTextFunction thetaA thetaLabelAlign thetaTxt
                    # translate thetaLabelPos
                    # applyStyle (thetaA ^. axisLabelStyle)
                    # fc black

  -- thetaLabelPos = V2 (s*x) (- view axisLabelGap thetaA) where
  thetaLabelPos = view xy_ (mkPolar (s*r + view axisLabelGap thetaA) x) where
    -- The angle on the axis the label is placed, doesn't make much
    -- sense right now.
    x = case thetaA ^. axisLabelPosition of
          MiddleAxisLabel -> quarterTurn
          LowerAxisLabel  -> zero
          UpperAxisLabel  -> halfTurn
  thetaTxt = thetaA ^. axisLabelText
  thetaLabelAlign = BaselineText

  -- Angular axis ticks ------------------------------------------------

  -- The positions of major and minor ticks along the angular axis
  majorTickThetas  = view majorTicksFunction thetaA (0,theta)
  majorTickThetas' = filter thetaInRange majorTickThetas
  minorTickThetas  = view minorTicksFunction thetaA majorTickThetas (0,theta)
  minorTickThetas' = filter thetaInRange minorTickThetas

  -- Major and minor ticks are placed along perimeter, facing the center
  -- of the axis. Ticks start of horizonal and are rotated to the
  -- correct position on the axis.
  thetaAxTicks      = thetaAxMajorTicks <> thetaAxMinorTicks
  thetaAxMajorTicks
    | thetaA ^. majorTicks . hidden = mempty
    | otherwise = F.foldMap (\phi -> thetaAxMajorTick # translateX (s*r) # rotate (phi@@rad)) majorTickThetas'
                       # applyStyle (thetaA ^. majorTicksStyle)
  thetaAxMinorTicks
    | thetaA ^. minorTicks . hidden = mempty
    | otherwise = F.foldMap (\phi -> thetaAxMinorTick # translateX (s*r) # rotate (phi@@rad)) minorTickThetas'
                       # applyStyle (thetaA ^. minorTicksStyle)

  -- The paths used for individual major and minor ticks
  thetaAxMajorTick = someThetaTick (thetaA ^. majorTicksAlignment) (thetaA ^. majorTicksLength)
  thetaAxMinorTick = someThetaTick (thetaA ^. minorTicksAlignment) (thetaA ^. minorTicksLength)

  someThetaTick tType d = case tType of
    TickSpec (fromRational -> aa) (fromRational -> bb)
             -> fromVertices [mkP2 (-d*bb) 0, mkP2 (d*aa) 0]
    AutoTick -> fromVertices [mkP2 (-d) 0   , mkP2 d 0     ]

  -- Angular grid lines ------------------------------------------------

  -- grid lines go from the centre of the axis to the perimeter
  thetaAxGridLines
    -- - | thetaA ^. gridLines . hidden = mempty
    | otherwise                = thetaMajorGridLines <> thetaMinorGridLines

  majorGridThetas = view majorGridLinesFunction thetaA majorTickThetas (0,theta)
  majorGridThetas' = filter thetaInRange majorGridThetas

  thetaMajorGridLines :: Diagram V2
  thetaMajorGridLines
    | thetaA ^. majorGridLines . hidden = mempty
    | otherwise = F.foldMap (\phi -> fromVertices [origin, mkP2 r 0] # rotate (phi@@rad)) majorGridThetas'
                    # transform t
                    # applyStyle (thetaA ^. majorGridLinesStyle)

  minorGridThetas    = view minorGridLinesFunction thetaA minorTickThetas (0,theta)
  minorGridThetas'   = filter thetaInRange minorGridThetas
  thetaMinorGridLines :: Diagram V2
  thetaMinorGridLines
    | thetaA ^. minorGridLines . hidden = mempty
    | otherwise = F.foldMap (\phi -> fromVertices [origin, mkP2 r 0] # rotate (phi@@rad)) minorGridThetas'
                    # transform t
                    # applyStyle (thetaA ^. minorGridLinesStyle)

  -- Angular tick labels -----------------------------------------------

  thetaAxTickLabels :: Diagram V2
  thetaAxTickLabels
    | thetaA ^. tickLabel . hidden = mempty
    | otherwise                    = F.foldMap thetaDrawTickLabel tickLabelThetas

  -- The positions of the tick labels.
  tickLabelThetas :: [(Double, String)]
  tickLabelThetas = view tickLabelFunction thetaA majorTickThetas' (0,theta)

  -- Draw a single tick label given the position and the string to use
  thetaDrawTickLabel :: (Double, String) -> Diagram V2
  thetaDrawTickLabel (x,label) =
    view tickLabelTextFunction thetaA a label
      # translate v
      # applyStyle (thetaA ^. tickLabelStyle)
      # fc black
        where v = mkPolar (s*r + view axisLabelGap thetaA) (x@@rad) ^. xy_
              -- a = BoxAlignedText (0.5-cos x/2) (0.5-sin x/2)
              a = BoxAlignedText 0.5 0.5

    -- line
    --   | a ^. axisLine . hidden = mempty
    --   | otherwise = F.foldMap mkline (map snd ys) -- merge with ticks?
    --          # transform t
    --          # stroke
    --          -- # applyStyle (a ^. axisLine e . axisArrowOpts . _Just . shaftStyle)
    --          # lineCap LineCapSquare
    --   where
    --     -- TODO: Arrow for R3
    --     mkline y = pathFromVertices
    --      $ map (\x -> over lensP ((el e .~ x) . (el eO .~ y)) p) [x0, x1] :: Path v n
