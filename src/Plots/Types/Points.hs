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
{-# LANGUAGE AllowAmbiguousTypes       #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.Types.Points
  (  -- * Polar scatter plot
     GPointsPlot
   , mkPointsPlot

     -- * Lenses
   , doFill

     -- * Points plot
  , pointsPlot
  , pointsPlot'
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import           Control.Monad.State.Lazy

import           Data.Typeable

import           Diagrams.Prelude
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Coordinates.Polar

import           Plots.Style
import           Plots.Types
import           Plots.Axis

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

instance (v ~ V2, TypeableFloat n, Renderable (Path v n) b)
    => Plotable (GPointsPlot n) b where
  renderPlotable _ sty GPointsPlot {..} =
      mconcat [marker # applyMarkerStyle sty # scale 0.1 # moveTo (p2 (r*(cosA theta),r*(sinA theta)))| (r,theta) <- sPoints]
   <> if sFill
         then doline <> doarea
      else mempty
      where
       marker = sty ^. plotMarker
       doline = fromVertices (map p2 [(r*(cosA theta),r*(sinA theta)) | (r,theta)  <- sPoints]) # mapLoc closeLine # stroke # applyLineStyle sty
       doarea = fromVertices (map p2 [(r*(cosA theta),r*(sinA theta)) | (r,theta)  <- sPoints]) # mapLoc closeLine # stroke # lw none # applyAreaStyle sty

  defLegendPic sty GPointsPlot {..}
      = sty ^. plotMarker
         & applyMarkerStyle sty

------------------------------------------------------------------------
-- Points plot
------------------------------------------------------------------------

-- | Plot a polar scatter plot given a list of radius and angle.
mkPointsPlot :: [(n, Angle n)] -> GPointsPlot n
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

instance HasPoints (Plot (GPointsPlot n) b) n where
  pts = rawPlot

------------------------------------------------------------------------
-- Points plot
------------------------------------------------------------------------

-- $ points plot
-- Points plot display data as scatter (dots) on polar co-ord.
-- Points plots have the following lenses:
--
-- @
-- * 'doFill' :: 'Lens'' ('BoxPlot' v n) 'Bool' - False
-- @
--
-- | Add a 'PointsPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = polarAxis ~&
--     pointsPlot data1
-- @
--
-- === __Example__
--
-- <<diagram=points&width=300>>
--
-- > myaxis :: Axis B Polar Double
-- > myaxis = polarAxis &~ do
-- >    pointsPlot mydata1
-- >    pointsPlot mydata2
-- >    pointsPlot mydata3

pointsPlot
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPointsPlot n) b)
  => [(n,Angle n)] -> State (Plot (GPointsPlot n) b) () -> m ()
pointsPlot ds = addPlotable (mkPointsPlot ds)

-- | Make a 'PointsPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = polarAxis &~ do
--     pointsPlot' pointData1 $ do
--       addLegendEntry "data 1"
--       doFill .= True
-- @

pointsPlot'
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (GPointsPlot n) b)
  => [(n,Angle n)] -> m ()
pointsPlot' ds = addPlotable' (mkPointsPlot ds)
