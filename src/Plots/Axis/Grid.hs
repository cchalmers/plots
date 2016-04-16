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

    -- * Extra traversals
  , gridLineStyle
  , gridLineVisible

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

-- | A grid line function takes the positions of the respective ticks
--   (minor ticks for minor grid lines, major ticks for major grid
--   lines) and the bounds of the axis and returns the positions of the
--   grid lines.
--
--   These functions are used in conjuction with 'majorGridLineFunction'
--   and 'minorGridLineFunction' to control how the lines are drawn.
type GridLineFunction n = [n] -> (n, n) -> [n]

data GridLines v n = GridLines
  { magFun     :: GridLineFunction n
  , migFun     :: GridLineFunction n
  , magStyle   :: Style v n
  , migStyle   :: Style v n
  , magVisible :: Bool
  , migVisible :: Bool
  } deriving Typeable

type instance V (GridLines v n) = v
type instance N (GridLines v n) = n

makeLensesFor
  [ ("magFun",     "_majorGridLineFunction")
  , ("migFun",     "_minorGridLineFunction")
  , ("magStyle",   "_majorGridLineStyle")
  , ("migStyle",   "_minorGridLineStyle")
  , ("magVisible", "_majorGridLineVisible")
  , ("migVisible", "_minorGridLineVisible")
  -- for traversals
  , ("magStyle", "_gridLineStyle")
  , ("migStyle", "_gridLineStyle")
  , ("magVisible", "_gridLineVisible")
  , ("migVisible", "_gridLineVisible")
  ]
  ''GridLines

class HasGridLines f a where
  -- | The options for how to draw the grid lines. This can be used on
  --   various levels of the axis:
  --
  -- @
  -- 'gridLines' :: 'Traversal'' ('Axis' b c n)       ('GridLines' ('BaseSpace' c) n)
  -- 'gridLines' :: 'Lens''      ('SingleAxis' b v n) ('GridLines' v n)
  -- 'gridLines' :: 'Lens''      ('GridLines' v n)    ('GridLines' v n)
  -- @
  gridLines :: LensLike' f a (GridLines (V a) (N a))

  -- | The function to calculate location of the major grid lines given
  --   location of the major ticks and bounds.
  majorGridLineFunction :: Functor f => LensLike' f a (GridLineFunction (N a))
  majorGridLineFunction = gridLines . _majorGridLineFunction

  -- | The function to calculate location of the minor grid lines given
  --   location of the minor ticks and bounds.
  minorGridLineFunction :: Functor f => LensLike' f a (GridLineFunction (N a))
  minorGridLineFunction = gridLines . _minorGridLineFunction

  -- | The style applied to the major grid lines.
  majorGridLineStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  majorGridLineStyle = gridLines . _majorGridLineStyle

  -- | The style applied to the minor grid lines.
  minorGridLineStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  minorGridLineStyle = gridLines . _minorGridLineStyle

  -- | Whether the major grid lines should be visible.
  --
  --   Default is 'True'.
  majorGridLineVisible :: Functor f => LensLike' f a Bool
  majorGridLineVisible = gridLines . _majorGridLineVisible

  -- | Whether the minor grid lines should be visible.
  --
  --   Default is 'False'.
  minorGridLineVisible :: Functor f => LensLike' f a Bool
  minorGridLineVisible = gridLines . _minorGridLineVisible

instance HasGridLines f (GridLines v n) where
  gridLines = id

instance (Typeable n, Floating n) => Default (GridLines v n) where
  def = GridLines
    { migFun     = onTicksGridLineFunction
    , magFun     = onTicksGridLineFunction
    , migStyle   = mempty # lwO 0.4
    , magStyle   = mempty # lwO 0.1
    , magVisible = True
    , migVisible = False
    }

instance Typeable n => HasStyle (GridLines v n) where
  applyStyle s = gridLineStyle %~ applyStyle s

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
gridLineVisible :: (HasGridLines f a, Applicative f) => LensLike' f a Bool
gridLineVisible = gridLines . _gridLineVisible

-- | Hide both major and minor grid lines.
--
-- @
-- 'hideGridLines' :: 'Axis' b c n       -> 'Axis' b c n
-- 'hideGridLines' :: 'SingleAxis' b c n -> 'SingleAxis' b c n
-- 'hideGridLines' :: 'GridLines' b c n  -> 'GirdLines' b c n
-- @
--
--   See 'HasGridLines' for more advanced visibility options.
hideGridLines :: (HasGridLines Identity a, MonadState a m) => m ()
hideGridLines = gridLineVisible .= False

-- | Show both major and minor grid lines.
--
-- @
-- 'showGridLines' :: 'Axis' b c n       -> 'Axis' b c n
-- 'showGridLines' :: 'SingleAxis' b c n -> 'SingleAxis' b c n
-- 'showGridLines' :: 'GridLines' b c n  -> 'GirdLines' b c n
-- @
--
--   See 'HasGridLines' for more advanced visibility options.
showGridLines :: (HasGridLines Identity a, MonadState a m) => m ()
showGridLines = gridLineVisible .= True

-- | Traversal over both the major and minor grid styles. This can be used at seversal levels in the Axis:
gridLineStyle :: (HasGridLines f a, Applicative f) => LensLike' f a (Style (V a) (N a))
gridLineStyle = gridLines . _gridLineStyle
