{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Grid
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Lines that go along the axis. Supports major and minor gird lines
-- separately for each axis.
--
----------------------------------------------------------------------------
module Plots.Axis.Grid
  ( -- * Grid lines
    GridLines
  , HasGridLines (..)

  , MajorGridLines
  , HasMajorGridLines (..)
  , MinorGridLines
  , HasMinorGridLines (..)

    -- * Extra traversals
  , gridLinesStyle
  , gridLinesVisible

  , hideGridLines
  , showGridLines

    -- * Grid line functions
  , GridLineFunction
  , onTicksGridLineFunction
  , emptyGridLineFunction

  ) where

import           Control.Lens        hiding (( # ))
import           Data.Data
import           Data.Default
import           Control.Monad.State

import           Diagrams.Prelude
import           Plots.Types

-- | A grid line function takes the positions of the respective ticks
--   (minor ticks for minor grid lines, major ticks for major grid
--   lines) and the bounds of the axis and returns the positions of the
--   grid lines.
--
--   These functions are used in conjuction with 'majorGridLineFunction'
--   and 'minorGridLineFunction' to control how the lines are drawn.
type GridLineFunction n = [n] -> (n, n) -> [n]

------------------------------------------------------------------------
-- Major grid lines
------------------------------------------------------------------------

data MajorGridLines v n = MajorGridLines
  { magFun     :: GridLineFunction n
  , magStyle   :: Style v n
  , magVisible :: Bool
  } deriving Typeable

type instance V (MajorGridLines v n) = v
type instance N (MajorGridLines v n) = n

class HasMajorGridLines f a where
  -- | The options for how to draw the grid lines. This can be used on
  --   various levels of the axis:
  --
  -- @
  -- 'majorGridLines' :: 'Traversal'' ('Axis' b c n)       ('GridLines' ('BaseSpace' c) n)
  -- 'majorGridLines' :: 'Lens''      ('SingleAxis' b v n) ('GridLines' v n)
  -- 'majorGridLines' :: 'Lens''      ('GridLines' v n)    ('GridLines' v n)
  -- @
  majorGridLines :: LensLike' f a (MajorGridLines (V a) (N a))

  -- | The function to calculate location of the major grid lines given
  --   location of the major ticks and bounds.
  majorGridLinesFunction :: Functor f => LensLike' f a (GridLineFunction (N a))
  majorGridLinesFunction = majorGridLines . lens magFun (\gl maf -> gl {magFun = maf})

  -- | The style applied to the major grid lines.
  majorGridLinesStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  majorGridLinesStyle = majorGridLines . lens magStyle (\gl sty -> gl {magStyle = sty})

instance HasMajorGridLines f (MajorGridLines v n) where
  majorGridLines = id

instance (Typeable n, Floating n) => Default (MajorGridLines v n) where
  def = MajorGridLines
    { magFun     = onTicksGridLineFunction
    , magStyle   = mempty # lwO 0.8
    , magVisible = True
    }

instance HasVisibility (MajorGridLines v n) where
  visible = lens magVisible (\gl b -> gl {magVisible = b})

instance Typeable n => HasStyle (MajorGridLines v n) where
  applyStyle s = majorGridLinesStyle %~ applyStyle s

------------------------------------------------------------------------
-- Minor grid lines
------------------------------------------------------------------------

data MinorGridLines v n = MinorGridLines
  { migFun     :: GridLineFunction n
  , migStyle   :: Style v n
  , migVisible :: Bool
  } deriving Typeable

type instance V (MinorGridLines v n) = v
type instance N (MinorGridLines v n) = n

class HasMinorGridLines f a where
  -- | The options for how to draw the grid lines. This can be used on
  --   various levels of the axis:
  --
  -- @
  -- 'minorGridLines' :: 'Traversal'' ('Axis' b c n)       ('GridLines' ('BaseSpace' c) n)
  -- 'minorGridLines' :: 'Lens''      ('SingleAxis' b v n) ('GridLines' v n)
  -- 'minorGridLines' :: 'Lens''      ('GridLines' v n)    ('GridLines' v n)
  -- @
  minorGridLines :: LensLike' f a (MinorGridLines (V a) (N a))

  -- | The function to calculate location of the minor grid lines given
  --   location of the minor ticks and bounds.
  minorGridLinesFunction :: Functor f => LensLike' f a (GridLineFunction (N a))
  minorGridLinesFunction = minorGridLines . lens migFun (\gl mif -> gl {migFun = mif})


  -- | The style applied to the minor grid lines.
  minorGridLinesStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  minorGridLinesStyle = minorGridLines . lens migStyle (\gl sty -> gl {migStyle = sty})

instance HasMinorGridLines f (MinorGridLines v n) where
  minorGridLines = id

instance (Typeable n, Floating n) => Default (MinorGridLines v n) where
  def = MinorGridLines
    { migFun     = onTicksGridLineFunction
    , migStyle   = mempty # lwO 0.5
    , migVisible = False
    }

-- | Hidden by default.
instance HasVisibility (MinorGridLines v n) where
  visible = lens migVisible (\gl b -> gl {migVisible = b})

instance Typeable n => HasStyle (MinorGridLines v n) where
  applyStyle s = minorGridLinesStyle %~ applyStyle s

------------------------------------------------------------------------
-- Grid lines helpers
------------------------------------------------------------------------

-- | Place grid lines at the same position as the respective ticks. This
--   is the 'Default'.
onTicksGridLineFunction :: GridLineFunction n
onTicksGridLineFunction = const

-- | The 'GridLineFunction' such that no grid lines appear.
--
--   See 'hideGridLines', 'majorGridLineVisible' or
--   'minorGridLineVisible' if you just want to hide the grid lines.
emptyGridLineFunction :: GridLineFunction n
emptyGridLineFunction _ _ = []

-- | Traversal over both the major and minor grid styles.
--
-- @
-- 'gridLinesVisible' :: 'Traversal'' ('Axis' b c n) 'Bool'
-- 'gridLinesVisible' :: 'Traversal'' ('SingleAxis' b v n) 'Bool'
-- 'gridLinesVisible' :: 'Traversal'' ('GridLines' v n) 'Bool'
-- @
gridLinesVisible :: (HasGridLines f a, Applicative f) => LensLike' f a Bool
gridLinesVisible = gridLines . vis where
  vis :: Traversal' (GridLines v n) Bool
  vis f a =
       (\m mn -> a & majorGridLines . visible .~ m & minorGridLines . visible .~ mn)
         <$> f (a ^. majorGridLines . visible) <*> f (a ^. minorGridLines . visible)

------------------------------------------------------------------------
-- Both grid lines
------------------------------------------------------------------------

-- | Type holding infomation about both major and minor grid lines.
data GridLines v n = GridLines
  { majGrid     :: MajorGridLines v n
  , minGrid     :: MinorGridLines v n
  } deriving Typeable

type instance V (GridLines v n) = v
type instance N (GridLines v n) = n

class (HasMinorGridLines f a, HasMajorGridLines f a) => HasGridLines f a where
  gridLines :: LensLike' f a (GridLines (V a) (N a))

instance Functor f => HasGridLines f (GridLines v n) where
  gridLines = id

instance (Typeable n, Floating n) => Default (GridLines v n) where
  def = GridLines
    { majGrid = def
    , minGrid = def
    }

instance Functor f => HasMajorGridLines f (GridLines v n) where
  majorGridLines = lens majGrid (\g a -> g {majGrid = a})

instance Functor f => HasMinorGridLines f (GridLines v n) where
  minorGridLines = lens minGrid (\g a -> g {minGrid = a})

instance Typeable n => HasStyle (GridLines v n) where
  applyStyle s = (majorGridLines %~ applyStyle s) . (minorGridLines %~ applyStyle s)

-- | Hide both major and minor grid lines.
--
-- @
-- 'hideGridLines' :: 'Axis' b c n       -> 'Axis' b c n
-- 'hideGridLines' :: 'SingleAxis' b c n -> 'SingleAxis' b c n
-- 'hideGridLines' :: 'GridLines' b c n  -> 'GirdLines' b c n
-- @
hideGridLines :: (HasGridLines Identity a, MonadState a m) => m ()
hideGridLines = do
  minorGridLines . visible .= False
  majorGridLines . visible .= False

-- | Show both major and minor grid lines.
--
-- @
-- 'showGridLines' :: 'Axis' b c n       -> 'Axis' b c n
-- 'showGridLines' :: 'SingleAxis' b c n -> 'SingleAxis' b c n
-- 'showGridLines' :: 'GridLines' b c n  -> 'GirdLines' b c n
-- @
showGridLines :: (HasGridLines Identity a, MonadState a m) => m ()
showGridLines = do
  minorGridLines . visible .= True
  majorGridLines . visible .= True

-- | Traversal over both the major and minor grid styles. This can be used at seversal levels in the Axis:
gridLinesStyle :: (HasGridLines f a, Applicative f) => LensLike' f a (Style (V a) (N a))
gridLinesStyle = gridLines . styles where
  styles :: Traversal' (GridLines v n) (Style v n)
  styles f a =
    (\m mn -> a & majorGridLinesStyle .~ m & minorGridLinesStyle .~ mn)
      <$> f (a ^. majorGridLinesStyle) <*> f (a ^. minorGridLinesStyle)
