{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Plots.Types.Pie
  (  GPiePlot
   , mkWedgePlot
   , mkWedgePlotFrom
   , mkAnnularWedgePlotFrom
   , mkAnnularWedgePlot
   , strokeArc
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
-- import qualified Data.Foldable                   as F
import           Data.Typeable

import           Diagrams.Prelude hiding (r2)

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Coordinates.Polar

import           Plots.Themes
import           Plots.Types

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

mkWedgePlot :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> Angle n -> GPiePlot n
mkWedgePlot r a = GPiePlot
  { sLargeRadius    = r
  , sSmallRadius    = 0.01
  , sDirection      = xDir
  , sAngle          = a
  , sArc            = False
  }

mkWedgePlotFrom :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> Direction V2 n -> Angle n -> GPiePlot n
mkWedgePlotFrom  r d a = GPiePlot
  { sLargeRadius = r
  , sSmallRadius = 0.01
  , sDirection   = d
  , sAngle       = a
  , sArc         = False
  }
-- cannot use 0 as scale by 0
mkAnnularWedgePlot :: (RealFloat n, PointLike v n (Polar n), Num n)
                => n -> n -> Angle n -> GPiePlot n
mkAnnularWedgePlot r2 r1 a = GPiePlot
  { sLargeRadius    = r2
  , sSmallRadius    = r1
  , sDirection      = xDir
  , sAngle          = a
  , sArc            = False
  }

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
