{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Grid
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Low level module defining type for the axis grid.
--
----------------------------------------------------------------------------
module Plots.Axis.Grid
  ( -- * Grid lines
    GridLines
  , HasGridLines (..)
  , tickGridF
  , noGridF
  , gridLinesStyle
  ) where

import           Control.Lens     hiding (( # ))
import           Data.Data
import           Data.Default

import           Diagrams.Prelude

-- | A grid line function takes the positions of the respective ticks
--   (minor ticks for minor grid lines, major ticks for major grid
--   lines) and the bounds of the axis and returns the positions of the
--   grid lines.
type GridLinesFunction n = [n] -> (n, n) -> [n]

data GridLines v n = GridLines
  { magFun     :: GridLinesFunction n
  , migFun     :: GridLinesFunction n
  , magStyle :: Style v n
  , migStyle :: Style v n
  } deriving Typeable

type instance V (GridLines v n) = v
type instance N (GridLines v n) = n

class HasGridLines a where
  gridLines :: Lens' a (GridLines (V a) (N a))

  -- | The function to calculate location of the major grid lines given
  --   location of the major ticks and bounds.
  majorGridLinesFunction :: Lens' a (GridLinesFunction (N a))
  majorGridLinesFunction = gridLines . lens magFun (\gl maf -> gl {magFun = maf})

  -- | The function to calculate location of the minor grid lines given
  --   location of the minor ticks and bounds.
  minorGridLinesFunction :: Lens' a (GridLinesFunction (N a))
  minorGridLinesFunction = gridLines . lens migFun (\gl mif -> gl {migFun = mif})

  -- | The style applied to the major grid lines.
  majorGridLinesStyle :: Lens' a (Style (V a) (N a))
  majorGridLinesStyle = gridLines . lens magStyle (\gl sty -> gl {magStyle = sty})

  -- | The style applied to the minor grid lines.
  minorGridLinesStyle :: Lens' a (Style (V a) (N a))
  minorGridLinesStyle = gridLines . lens migStyle (\gl sty -> gl {migStyle = sty})

instance HasGridLines (GridLines v n) where
  gridLines = id

instance (Typeable n, Floating n) => Default (GridLines v n) where
  def = GridLines
    { migFun   = noGridF
    , magFun   = tickGridF
    , migStyle = mempty # lwO 0.4
    , magStyle = mempty # lwO 0.1
    }

instance Typeable n => HasStyle (GridLines v n) where
  applyStyle s = over gridLinesStyle (applyStyle s)

-- | Place grid lines at the same position as the respective ticks.
tickGridF :: GridLinesFunction n
tickGridF = const

-- | The no grid lines function.
noGridF :: GridLinesFunction n
noGridF _ _ = []

-- | Traversal over both the major and minor grid styles.
gridLinesStyle :: HasGridLines a => Traversal' a (Style (V a) (N a))
gridLinesStyle f a = (\m mn -> a & majorGridLinesStyle .~ m & minorGridLinesStyle .~ mn)
             <$> f (a ^. majorGridLinesStyle) <*> f (a ^. minorGridLinesStyle)

