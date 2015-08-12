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
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Plots.Types.Text
  ( TextPlot
  , TextOptions
  , mkTextPlot
  , setOptions
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import qualified Data.Foldable                   as F
import           Data.Typeable

import           Diagrams.Prelude
import           Diagrams.TwoD.Text

import           Diagrams.Coordinates.Isomorphic

import           Plots.Themes
import           Plots.Types

data TextPlot n = TextPlot
   { _tString        :: String
   , _textPoint     :: (n,n) 
   , _textOptions   :: TextOptions n
   }

data TextOptions n = TextOptions
   {  _optalignment      :: (n, n)
    , _optfontSize       :: n
    , _optfontSlant      :: FontSlant
    , _optfontWeight     :: FontWeight
   }

instance (Fractional n) => Default (TextOptions n) where
  def = TextOptions
   {  _optalignment      = (0.0, 0.0)
    , _optfontSize       = 0.4
    , _optfontSlant      = FontSlantNormal
    , _optfontWeight     = FontWeightNormal
   }

--     _font           = Font

makeLenses ''TextPlot
makeLenses ''TextOptions

type instance V (TextPlot n) = V2
type instance N (TextPlot n) = n

instance (Fractional n, OrderedField n, TypeableFloat n, Enum n) => Enveloped (TextPlot n) where
  getEnvelope = const mempty

-- #fsze #fslant #fwght
instance (Fractional n, Typeable b, TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
    => Plotable (TextPlot n) b where
  renderPlotable s v pp = alignedText a b str # fontSize (local fsze) 
                          # applyTextStyle pp
                          # transform (s^.specTrans)
                          # moveTo (p2 (x, y)) <> circle 1 # fc red
                          where
                             (x, y)   = (v ^. textPoint)
                             str      = v ^. tString
                             (a, b)   = v ^. textOptions ^. optalignment
                             fsze     = v ^. textOptions ^. optfontSize
                             fslant   = v ^. textOptions ^. optfontSlant
                             fwght    = v ^. textOptions ^. optfontWeight

  defLegendPic TextPlot {..} pp
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle pp

mkTextPlot :: (TypeableFloat n, Fractional n) => (n,n) -> String -> TextPlot n
mkTextPlot p1 f
  = TextPlot
   { _tString       = f
   , _textPoint     = p1  
   , _textOptions   = def
   }

------------------------------------------------------------------------
-- Text lenses
------------------------------------------------------------------------

class HasText a n | a -> n where
  txt :: Lens' a (TextPlot n)

  setOptions :: Lens' a (TextOptions n)
  setOptions = txt . lens _textOptions (\s b -> (s {_textOptions = b}))

instance HasText (TextPlot n) n where
  txt = id

instance HasText (PropertiedPlot (TextPlot n) b) n where
  txt = _pp
