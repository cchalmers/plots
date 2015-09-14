{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Pie
  (  -- * General pie plot
     GPiePlot

     -- * Wedge plot
   , mkWedgePlot
   , mkWedgePlotFrom

     -- * Annular wedge plot
   , mkAnnularWedgePlotFrom
   , mkAnnularWedgePlot

     -- * Lenses
   , strokeArc

    -- * Wedge
  , wedgePlot
  , wedgePlotFrom
  , wedgePlotFrom'

    -- * Annular wedge
  , annularWedgePlot
  , annularWedgePlotFrom
  , annularWedgePlotFrom'
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import           Control.Monad.State.Lazy

import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude hiding (r2)

import           Plots.Themes
import           Plots.Types
import           Plots.API

------------------------------------------------------------------------
-- General pie plot
------------------------------------------------------------------------

data GPiePlot n = GPiePlot
  { sLargeRadius :: n
  , sSmallRadius :: n
  , sDirection   :: Direction V2 n
  , sAngle       :: Angle n
  , sArc         :: Bool
  } deriving Typeable

type instance V (GPiePlot n)  = V2
type instance N (GPiePlot n)  = n

instance (OrderedField n) => Enveloped (GPiePlot n) where
  getEnvelope GPiePlot {..} = mempty

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GPiePlot n) b where
  renderPlotable _s GPiePlot {..} pp =
      annularWedge sLargeRadius sSmallRadius sDirection sAngle
        # lw none
        # applyAreaStyle pp
   <> if sArc
        then arc' sLargeRadius sDirection sAngle
               # applyLineStyle pp
          <> arc' sLargeRadius sDirection sAngle
               # applyLineStyle pp
        else mempty

  defLegendPic GPiePlot {..} pp
      = square 5 # applyAreaStyle pp

------------------------------------------------------------------------
-- Wedge plot
------------------------------------------------------------------------

-- | Plot a wedge given radius and angle with direction xDir.
mkWedgePlot :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> Angle n -> GPiePlot n
mkWedgePlot r a = GPiePlot
  { sLargeRadius    = r
  , sSmallRadius    = 0.01
  , sDirection      = xDir
  , sAngle          = a
  , sArc            = False
  }

-- | Plot a wedge given radius, angle and direction.
mkWedgePlotFrom :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> Direction V2 n -> Angle n -> GPiePlot n
mkWedgePlotFrom  r d a = GPiePlot
  { sLargeRadius = r
  , sSmallRadius = 0.01
  , sDirection   = d
  , sAngle       = a
  , sArc         = False
  }

------------------------------------------------------------------------
-- Annular wedge plot
------------------------------------------------------------------------

-- | Plot a annular wedge given radius and angle with direction xDir.
mkAnnularWedgePlot :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> n -> Angle n -> GPiePlot n
mkAnnularWedgePlot r2 r1 a = GPiePlot
  { sLargeRadius    = r2
  , sSmallRadius    = r1
  , sDirection      = xDir
  , sAngle          = a
  , sArc            = False
  }

-- | Plot a annular wedge given radius, angle and direction.
mkAnnularWedgePlotFrom :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> n -> Direction V2 n -> Angle n -> GPiePlot n
mkAnnularWedgePlotFrom  r2 r1 d a = GPiePlot
  { sLargeRadius = r2
  , sSmallRadius = r1
  , sDirection   = d
  , sAngle       = a
  , sArc         = False
  }

------------------------------------------------------------------------
-- Pie lenses
------------------------------------------------------------------------

class HasPie a n | a -> n where
  pie :: Lens' a (GPiePlot n)

  strokeArc :: Lens' a Bool
  strokeArc = pie . lens sArc (\s b -> (s {sArc = b}))

instance HasPie (GPiePlot n) n where
  pie = id

instance HasPie (PropertiedPlot (GPiePlot n) b) n where
  pie = _pp

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
