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
--   intended to be draw directly. Instead use 'piePlot.
data Wedge n = Wedge
  { sEndR   :: n
  , sStartR :: n
  , sOffset :: n
  , sDir    :: Direction V2 n
  , sWidth  :: Angle n
  } deriving Typeable

type instance V (Wedge n)  = V2
type instance N (Wedge n)  = n

instance RealFloat n => Enveloped (Wedge n) where
  getEnvelope Wedge {..} = getEnvelope shape # translate off where
    shape
      | sStartR == 0 = wedge sEndR sDir sWidth :: Path V2 n
      | otherwise    = annularWedge sEndR sStartR sDir sWidth
    off
      | sOffset == 0 = zero
      | otherwise    = sOffset *^ fromDir (rotate (sWidth ^/ 2) sDir)

instance (TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (Wedge n) b where
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
  :: Num n
  => Direction V2 n -- ^ starting direction
  -> Angle n        -- ^ width of wedge
  -> Wedge n        -- ^ resulting wedge
mkWedge d theta = Wedge
  { sEndR   = 1
  , sStartR = 0
  , sOffset = 0
  , sDir    = d
  , sWidth  = theta
  }

class HasWedge f a where
  -- | Description on how to draw a wedge.
  pieWedge :: LensLike' f a (Wedge (N a))

  -- | The outside radius of the wedge. Default is @1@.
  wedgeOuterRadius :: Functor f => LensLike' f a (N a)
  wedgeOuterRadius = pieWedge . lens sEndR (\p r -> p {sEndR = r})

  -- | The inside radius of the wedge. Default is $0$.
  wedgeInnerRadius :: Functor f => LensLike' f a (N a)
  wedgeInnerRadius = pieWedge . lens sStartR (\p r -> p {sStartR = r})

  -- | The offset of the wedge from the center.
  wedgeOffset :: Functor f => LensLike' f a (N a)
  wedgeOffset = pieWedge . lens sOffset (\p x -> p {sOffset = x})

  -- | The width of the wedge, starting from the 'wedgeDirection'.
  wedgeWidth :: Functor f => LensLike' f a (Angle (N a))
  wedgeWidth = pieWedge . lens sWidth (\p x -> p {sWidth = x})

  -- | The inititial direction of the wedge.
  wedgeDirection :: Functor f => LensLike' f a (Direction V2 (N a))
  wedgeDirection = pieWedge . lens sDir (\p x -> p {sDir = x})

instance HasWedge f (Wedge n) where
  pieWedge = id

instance (Functor f, HasWedge f a) => HasWedge f (Plot a b) where
  pieWedge = rawPlot . pieWedge

instance Applicative f => HasWedge f (PieState b n a) where
  pieWedge = stateMods . traversed . _2 . pieWedge

instance (Applicative f, Typeable b, v ~ V2, Typeable n)
    => HasWedge f (DynamicPlot b v n) where
  pieWedge = (dynamicPlot :: Traversal' (DynamicPlot b v n) (Plot (Wedge n) b))
           . pieWedge

instance (v ~ V2, Applicative f, Typeable n)
    => HasWedge f (StyledPlot b v n) where
  pieWedge = (styledPlot :: Traversal' (StyledPlot b v n) (Wedge n))

instance (BaseSpace c ~ V2, Settable f, Typeable n)
    => HasWedge f (Axis b c n) where
  pieWedge = finalPlots . pieWedge

------------------------------------------------------------------------
-- Full pie
------------------------------------------------------------------------

-- | The state used to draw a part chart made of multiple pie wedges.
data PieState b n a = PieState
  { psMods  :: [(a, Plot (Wedge n) b)] -- non-empty list
  }

type instance V (PieState b n a) = V2
type instance N (PieState b n a) = n

-- internal lens
stateMods :: Lens' (PieState b n a) [(a, Plot (Wedge n) b)]
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
onWedges :: (a -> State (Plot (Wedge n) b) ()) -> State (PieState b n a) ()
onWedges f = stateMods %= map (\(a, p) -> (a, execState (f a) p))

-- | Add a legend entry for each item given a function that extracts the
--   item's name.
wedgeKeys :: Num n => (a -> String) -> State (PieState b n a) ()
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
-- >   wedgeInnerRadius .= 0.5
-- >   hide (axes . traversed)
--
-- > piePlotExample = renderAxis piePlotAxis
piePlot
  :: (MonadState (Axis b Polar n) m,
      Plotable (Wedge n) b,
      Foldable f)
  => f a    -- ^ data for each wedge
  -> (a -> n) -- ^ extract weight of each wedge
  -> State (PieState b n a) ()
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
  :: (MonadState (Axis b Polar n) m,
      Plotable (Wedge n) b,
      Foldable f)
  => f n    -- ^ weight of each wedge
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
  :: (v ~ BaseSpace c, v ~ V2,
      PointLike v n (Polar n),
      MonadState (Axis b c n) m,
      Plotable (Wedge n) b
      )
  => Direction V2 n -> Angle n -> State (Plot (Wedge n) b) () -> m ()
wedgePlot r theta = addPlotable (mkWedge r theta)

