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

module Plots.Types.Points
  (  -- * Polar scatter plot  
     GPointsPlot
   , mkPointsPlot

     -- * Lenses
   , doFill
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))

import           Data.Typeable

import           Diagrams.Prelude
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Coordinates.Polar

import           Plots.Themes
import           Plots.Types

------------------------------------------------------------------------
-- GPoints plot
------------------------------------------------------------------------

data GPointsPlot n = GPointsPlot
  { sPoints      :: [(n, Angle n)]
  , sFill         :: Bool
  } deriving Typeable

-- options for style and transform.
-- lenses for style and transform,
-- scatter plot for example.

type instance V (GPointsPlot n)  = V2
type instance N (GPointsPlot n)  = n

instance (OrderedField n) => Enveloped (GPointsPlot n) where
  getEnvelope GPointsPlot {..} = mempty

instance (v ~ V2, Typeable b, TypeableFloat n, Renderable (Path v n) b)
    => Plotable (GPointsPlot n) b where
  renderPlotable _ GPointsPlot {..} pp =
      mconcat [marker # applyMarkerStyle pp # scale 0.1 # moveTo (p2 (r*(cosA theta),r*(sinA theta)))| (r,theta) <- sPoints]
   <> if sFill
         then doline <> doarea
      else mempty
      where
       marker = pp ^. plotMarker
       doline = fromVertices (map p2 [(r*(cosA theta),r*(sinA theta)) | (r,theta)  <- sPoints]) # mapLoc closeLine # stroke # applyLineStyle pp
       doarea = fromVertices (map p2 [(r*(cosA theta),r*(sinA theta)) | (r,theta)  <- sPoints]) # mapLoc closeLine # stroke # lw none # applyBarStyle pp

  defLegendPic GPointsPlot {..} pp
      = pp ^. plotMarker
         & applyMarkerStyle pp

------------------------------------------------------------------------
-- Points plot
------------------------------------------------------------------------

-- | Plot a polar scatter plot given a list of radius and angle.
mkPointsPlot :: (RealFloat n, PointLike V2 n (Polar n), Num n)
                => [(n, Angle n)] -> GPointsPlot n
mkPointsPlot ds = GPointsPlot
  { sPoints      =  ds
  , sFill        =  False
  }

------------------------------------------------------------------------
-- Points lenses
------------------------------------------------------------------------

class HasPoints a n | a -> n where
  pts :: Lens' a (GPointsPlot n)

  doFill :: Lens' a Bool
  doFill  = pts . lens sFill (\s b -> (s {sFill = b}))

instance HasPoints (GPointsPlot n) n where
  pts = id

instance HasPoints (PropertiedPlot (GPointsPlot n) b) n where
  pts = _pp
