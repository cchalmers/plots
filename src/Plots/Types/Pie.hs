{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE StandaloneDeriving        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.Line
-- Copyright   :  (C) 2016 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A pie plot is a circular statistical graphic, which is divided into
-- slices to illustrate numerical proportion.
--
-- <<diagrams/src_Plots_Types_Pie_piePlotExample.svg#diagram=piePlotExample&height=350>>
--
-- (see 'piePlot' example for code to make this plot)
--
----------------------------------------------------------------------------

module Plots.Types.Pie
  ( -- * Pie plot
    PieState
  , piePlot
  , piePlot'
  , onWedges
  , wedgeKeys

    -- * Wedges
  , Wedge
  , mkWedge
  , HasWedge (..)
  , wedgePlot

  ) where

import           Control.Monad.State.Lazy

import           Data.Typeable
import qualified Data.Foldable as F
import qualified Data.List as List

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude hiding (r2)

import           Plots.Style
import           Plots.Types
import           Plots.Axis

------------------------------------------------------------------------
-- Pie wedge
------------------------------------------------------------------------

-- | Contains information to draw a single wedge of a pie. It is not
--   intended to be drawn directly. Instead use 'piePlot'.
data Wedge = Wedge
  { sEndR   :: Double
  , sStartR :: Double
  , sOffset :: Double
  , sDir    :: Direction V2 Double
  , sWidth  :: Angle Double
  } deriving Typeable

type instance V Wedge = V2
type instance N Wedge = Double

instance Enveloped Wedge where
  getEnvelope Wedge {..} = getEnvelope shape # translate off where
    shape
      | sStartR == 0 = wedge sEndR sDir sWidth :: Path V2 Double
      | otherwise    = annularWedge sEndR sStartR sDir sWidth
    off
      | sOffset == 0 = zero
      | otherwise    = sOffset *^ fromDir (rotate (sWidth ^/ 2) sDir)

instance Plotable Wedge where
  renderPlotable s sty Wedge {..} =
    shape
      # applyAreaStyle sty
      # translate off
      # transform (s^.specTrans)
    where
      shape
        | sStartR == 0 = wedge sEndR sDir sWidth
        | otherwise    = annularWedge sEndR sStartR sDir sWidth
      off
        | sOffset == 0 = zero
        | otherwise    = sOffset *^ fromDir (rotate (sWidth ^/ 2) sDir)

  defLegendPic sty Wedge {..}
      = square 5 # applyAreaStyle sty

-- | Create a pie wedge with unit radius, starting at direction @d@ with
--   width @theta@.
mkWedge
  :: Direction V2 Double -- ^ starting direction
  -> Angle Double        -- ^ width of wedge
  -> Wedge               -- ^ resulting wedge
mkWedge d theta = Wedge
  { sEndR   = 1
  , sStartR = 0
  , sOffset = 0
  , sDir    = d
  , sWidth  = theta
  }

class HasWedge f a where
  -- | Description on how to draw a wedge.
  pieWedge :: LensLike' f a Wedge

  -- | The outside radius of the wedge. Default is @1@.
  wedgeOuterRadius :: Functor f => LensLike' f a Double
  wedgeOuterRadius = pieWedge . lens sEndR (\p r -> p {sEndR = r})

  -- | The inside radius of the wedge. Default is $0$.
  wedgeInnerRadius :: Functor f => LensLike' f a Double
  wedgeInnerRadius = pieWedge . lens sStartR (\p r -> p {sStartR = r})

  -- | The offset of the wedge from the center.
  wedgeOffset :: Functor f => LensLike' f a Double
  wedgeOffset = pieWedge . lens sOffset (\p x -> p {sOffset = x})

  -- | The width of the wedge, starting from the 'wedgeDirection'.
  wedgeWidth :: Functor f => LensLike' f a (Angle Double)
  wedgeWidth = pieWedge . lens sWidth (\p x -> p {sWidth = x})

  -- | The inititial direction of the wedge.
  wedgeDirection :: Functor f => LensLike' f a (Direction V2 Double)
  wedgeDirection = pieWedge . lens sDir (\p x -> p {sDir = x})

instance HasWedge f Wedge where
  pieWedge = id

instance (Functor f, HasWedge f a) => HasWedge f (Plot a) where
  pieWedge = rawPlot . pieWedge

instance Applicative f => HasWedge f (PieState a) where
  pieWedge = stateMods . traversed . _2 . pieWedge

instance (Applicative f, v ~ V2) => HasWedge f (DynamicPlot v) where
  pieWedge = (dynamicPlot :: Traversal' (DynamicPlot v) (Plot Wedge))
           . pieWedge

instance (v ~ V2, Applicative f) => HasWedge f (StyledPlot v) where
  pieWedge = (styledPlot :: Traversal' (StyledPlot v) Wedge)

instance (BaseSpace c ~ V2, Settable f) => HasWedge f (Axis c) where
  pieWedge = finalPlots . pieWedge

------------------------------------------------------------------------
-- Full pie
------------------------------------------------------------------------

-- | The state used to draw a part chart made of multiple pie wedges.
data PieState a = PieState
  { psMods  :: [(a, Plot Wedge)] -- non-empty list
  }

type instance V (PieState a) = V2
type instance N (PieState a) = Double

-- internal lens
stateMods :: Lens' (PieState a) [(a, Plot Wedge)]
stateMods = lens psMods (\ps ms -> ps {psMods = ms})

-- -- | The direction for the first entry in the pie. Default is 'xDir'.
-- startingDirection :: Lens' (PieState b n a) (Direction V2 n)
-- startingDirection = lens psStart (\ps d -> ps {psStart = d})

-- -- | The ending direction of the final wedge. This can be used to make a
-- finalDirection ::

-- | Modify the state for each wedge given the data entry.
--
--   Some common lenses to use on the 'Wedge':
--
--       * 'plotColour' - change the colour of the bars
--
--       * 'areaStyle' - modify the style of the bars
--
--       * 'key' - add a legend entry for that group of bars
--
--       * 'wedgeOffset' - the offset of the wedge from the center
--
onWedges :: (a -> State (Plot Wedge) ()) -> State (PieState a) ()
onWedges f = stateMods %= map (\(a, p) -> (a, execState (f a) p))

-- | Add a legend entry for each item given a function that extracts the
--   item's name.
wedgeKeys :: Num n => (a -> String) -> State (PieState a) ()
wedgeKeys f = onWedges $ \a -> key (f a)

-- | Make a pie plot from a list of data by making a series of wedge
--   plots.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Pie_piePlotExample.svg#diagram=piePlotExample&height=350>>
--
-- > import Plots
-- >
-- > pieData = [("red", 3), ("blue", 4), ("green", 2), ("purple", 5)]
-- >
-- > piePlotAxis :: Axis B Polar Double
-- > piePlotAxis = polarAxis &~ do
-- >   piePlot pieData snd $ wedgeKeys fst
-- >   hide (axes . traversed)
--
-- > piePlotExample = renderAxis piePlotAxis
piePlot
  :: (MonadState (Axis Polar) m, F.Foldable f)
  => f a           -- ^ data for each wedge
  -> (a -> Double) -- ^ extract weight of each wedge
  -> State (PieState a) ()
  -> m ()
piePlot (F.toList -> as) f st = F.forM_ ps addPlot
  where
    -- calculate pie widths
    ns = map f as
    x  = F.sum ns
    wedges = snd $ List.mapAccumR wedgeAccum xDir as
    wedgeAccum d a = (d', wdg)
      where theta = (f a / x) @@ turn
            d'    = d # rotate theta
            wdg   = mkWedge d theta

    -- run pie state
    ps  = map snd . psMods $ execState st ps0
    ps0 = PieState { psMods = zip as (map mkPlot wedges) }

-- | Make a pie plot from list of values without any changes.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Pie_pieExample'.svg#diagram=pieExample'&height=350>>
--
-- > import Plots
-- >
-- > piePlotAxis' :: Axis B Polar Double
-- > piePlotAxis' = polarAxis &~ do
-- >   piePlot' [1,3,5,2]
-- >   wedgeInnerRadius .= 0.5
-- >   hide (axes . traversed)
--
-- > pieExample' = renderAxis piePlotAxis'
piePlot'
  :: (MonadState (Axis Polar) m, F.Foldable f)
  => f Double    -- ^ weight of each wedge
  -> m ()
piePlot' ns = piePlot ns id (return ())

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

-- | Add a single 'PiePlot' to the 'AxisState' from a data set.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Pie_wedgeExample.svg#diagram=wedgeExample&height=350>>
--
-- > import Plots
-- >
-- > wedgePlotAxis :: Axis B Polar Double
-- > wedgePlotAxis = polarAxis &~ do
-- >   wedgePlot xDir (38@@deg) $ key "wedge"
--
-- > wedgeExample = renderAxis wedgePlotAxis
wedgePlot
  :: (BaseSpace c ~ V2,
      PointLike v Double (Polar Double),
      MonadState (Axis c) m
      )
  => Direction V2 Double -> Angle Double -> State (Plot Wedge) () -> m ()
wedgePlot r theta = addPlotable (mkWedge r theta)

