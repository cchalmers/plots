{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Plots.Legend
 ( -- * Legend entries
   LegendEntry
 , legendText
 , legendPic
   -- * Legend configuration
 , legendPosition
 , legendOrientation
 , Legend
 , drawLegend
 ) where

import Diagrams.Prelude as D
import Control.Lens       hiding ((#))
import Diagrams.TwoD.Text
import Data.Typeable
import Data.Default

import Plots.Types
import Plots.Themes

data Legend b = Legend
  { _legendPosition    :: P2
  , _legendStyle       :: Style R2
  , _legendOrientation :: Orientation
  } deriving Typeable

type instance V (Legend b) = R2

makeLenses ''Legend

instance Default (Legend b) where
  def = Legend origin mempty Verticle

instance HasStyle (Legend b) where
  applyStyle sty = over legendStyle (applyStyle sty)

drawLegend :: (Renderable (Path R2) b, Renderable Text b)
           => Legend b -> [GenericPlot b v] -> Diagram b R2
drawLegend l ps = orient (l^.legendOrientation) hcat vcat $ concatMap mkLabels ps
  where
    mkLabels p = map mkLabel (p^.legendEntries)
      where
        mkLabel entry = (text (entry^.legendText) <> rect 60 10 # lw D.none)
                     ||| (entry^.legendPic) (p^.themeEntry)
-- 
