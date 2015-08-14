{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Pie
  ( -- * Wedge
    wedgePlot
  , wedgePlotFrom
  , wedgePlotFrom'

    -- * Annular wedge
  , annularWedgePlot
  , annularWedgePlotFrom
  , annularWedgePlotFrom'
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable as F

import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Types
import           Plots.API
import           Plots.Types.Pie

------------------------------------------------------------------------
-- Wedge
------------------------------------------------------------------------

-- $ pieplot
-- Pie plots display data as wedges and annular wedges.
-- Pie plots have the following lenses:
--
-- @
-- * 'strokeArc' :: 'Lens'' ('BoxPlot' v n) 'Bool' - False
-- @

-- | Add a 'PiePlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = polarAxis ~&
--     wedgePlot data1
-- @
--
-- === __Example__
--
-- <<plots/variedpie.png#diagram=variedpie&width=300>>
--
-- @
--
-- myaxis = polarAxis &~ do
--
--     wedgePlotFrom' 7 xDir (2/11 @@ turn) $ do
--        strokeArc .= True
--        plotColor .= sRGB24  255 127 0
--     wedgePlotFrom' 4.3 (rotate (2/11 @@ turn) xDir) (4/11 @@ turn) $ do
--        strokeArc .= True
--        plotColor .=  sRGB24 166 86  40
--     wedgePlotFrom' 8.1 (rotate (6/11 @@ turn) xDir)  (3/11 @@ turn) $ do
--        strokeArc .= True
--        plotColor .= sRGB24 247 129 191
--     annularWedgePlotFrom' 3.1  0.1 (rotate (9/11 @@ turn) xDir)  (2/11 @@ turn) $ do
--        strokeArc .= True
--        plotColor .= sRGB24 117 69 69
--     annularWedgePlotFrom' 5.7  3.1 (rotate (9/11 @@ turn) xDir)  (2/11 @@ turn) $ do
--        strokeArc .= True
--        plotColor .= sRGB24 154 65 65
--     annularWedgePlotFrom' 9.5  5.7 (rotate (9/11 @@ turn) xDir)  (2/11 @@ turn) $ do
--        strokeArc .= True
--        plotColor .= sRGB24 198  49   49
-- @

wedgePlot
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPiePlot n) b,
      RealFloat n)
  => n -> Angle n -> m ()
wedgePlot r a = addPlotable (mkWedgePlot r a)

wedgePlotFrom
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPiePlot n) b,
      RealFloat n)
  => n -> Direction V2 n -> Angle n -> m ()
wedgePlotFrom r d a = addPlotable (mkWedgePlotFrom r d a)

-- | Make a wedge and take a 'State' on the plot to alter it's
--   options

wedgePlotFrom'
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPiePlot n) b,
      RealFloat n)
  => n -> Direction V2 n -> Angle n -> PlotState (GPiePlot n) b  -> m ()
wedgePlotFrom' r d a = addPlotable' (mkWedgePlotFrom r d a)

------------------------------------------------------------------------
-- Annular wedge
------------------------------------------------------------------------

annularWedgePlot
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPiePlot n) b,
      RealFloat n)
  => n -> n -> Angle n -> m ()
annularWedgePlot r1 r2 a = addPlotable (mkAnnularWedgePlot r1 r2 a)

annularWedgePlotFrom
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPiePlot n) b,
      RealFloat n)
  => n -> n -> Direction V2 n -> Angle n -> m ()
annularWedgePlotFrom r1 r2 d a = addPlotable (mkAnnularWedgePlotFrom r1 r2 d a)

-- | Make an annular wedge and take a 'State' on the plot to alter it's
--   options

annularWedgePlotFrom'
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPiePlot n) b,
      RealFloat n)
  => n -> n -> Direction V2 n -> Angle n -> PlotState (GPiePlot n) b -> m ()
annularWedgePlotFrom' r1 r2 d a = addPlotable' (mkAnnularWedgePlotFrom r1 r2 d a)
