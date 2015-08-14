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
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))

import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude hiding (r2)

import           Plots.Themes
import           Plots.Types

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
        # applyBarStyle pp
   <> if sArc
        then arc' sLargeRadius sDirection sAngle
               # applyLineStyle pp
          <> arc' sLargeRadius sDirection sAngle
               # applyLineStyle pp
        else mempty

  defLegendPic GPiePlot {..} pp
      = square 5 # applyBarStyle pp

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
