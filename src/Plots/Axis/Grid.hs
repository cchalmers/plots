{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Plots.Axis.Grid where

import Control.Lens hiding ((#))
import Data.Data
import Data.Default

import Diagrams.Prelude

-- | A grid line function takes the positions of the respective ticks
-- (minor ticks for minor grid lines, major ticks for major grid lines)
-- and the bounds of the axis and returns the positions of the grid
-- lines.
type GridLinesFunction n = [n] -> (n, n) -> [n]

data GridLines v n = GridLines
  { _majorGridF     :: GridLinesFunction n
  , _minorGridF     :: GridLinesFunction n
  , _majorGridStyle :: Style v n
  , _minorGridStyle :: Style v n
  } deriving Typeable

type instance V (GridLines v n) = v
type instance N (GridLines v n) = n

makeLenses ''GridLines

type AxisGridLines v n = v (GridLines v n)

instance (Typeable n, Floating n) => Default (GridLines v n) where
  def = GridLines
          { _majorGridF    = tickGridF
          , _minorGridF    = noGridF
          , _majorGridStyle = mempty # lwO 0.4
          , _minorGridStyle = mempty # lwO 0.1
          }

instance Typeable n => HasStyle (GridLines v n) where
  applyStyle s = over gridStyle (applyStyle s)

-- | Place grid lines at the same position as the ticks.
tickGridF :: GridLinesFunction n
tickGridF = const

-- | No grid lines.
noGridF :: GridLinesFunction n
noGridF _ = const []

-- | Traversal over both the major and minor grid styles.
gridStyle :: Traversal' (GridLines v n) (Style v n)
gridStyle f a = (\m mn -> a & majorGridStyle .~ m & minorGridStyle .~ mn)
             <$> f (a ^. majorGridStyle) <*> f (a ^. minorGridStyle)

