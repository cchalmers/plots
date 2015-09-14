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
  ( -- * Text plot
    TextPlot
  , TextOptions
  , mkTextPlot

    -- * Text lenses
  , setOptions

  -- * Text plot
  , textPlot
  , textPlot'
  , textPlotL

  , tString
  , textPoint
  , textOptions

  , optalignment
  , optfontSize
  , optfontSlant
  , optfontWeight
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))

-- import qualified Data.Foldable                   as F
import           Data.Typeable

import           Diagrams.Prelude
import           Diagrams.TwoD.Text

import           Plots.Style
import           Plots.Types
import           Plots.API
import           Control.Monad.State.Lazy

------------------------------------------------------------------------
-- Text data  & options
------------------------------------------------------------------------

data TextPlot n = TextPlot
   { _tString        :: String
   , _textPoint      :: (n,n)
   , _textOptions    :: TextOptions n
   } deriving Typeable

-- | Text alignment, font size, slant and weight
data TextOptions n = TextOptions
   {  _optalignment      :: (n, n)
    , _optfontSize       :: n
    , _optfontSlant      :: FontSlant
    , _optfontWeight     :: FontWeight
   }

-- need to implement an option for fonts

instance (Fractional n) => Default (TextOptions n) where
  def = TextOptions
   {  _optalignment      = (0.0, 0.0)
    , _optfontSize       = 0.4
    , _optfontSlant      = FontSlantNormal
    , _optfontWeight     = FontWeightNormal
   }

makeLenses ''TextPlot
makeLenses ''TextOptions

type instance V (TextPlot n) = V2
type instance N (TextPlot n) = n

instance (Fractional n, OrderedField n, TypeableFloat n, Enum n) => Enveloped (TextPlot n) where
  getEnvelope = const mempty


instance (Fractional n, Typeable b, TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
    => Plotable (TextPlot n) b where
  renderPlotable s v pp = alignedText a b str # fontSize (local fsze)
                          # applyTextStyle pp
                          # transform (s^.specTrans)

                          where
                             -- (x, y)   = (v ^. textPoint)
                             str      = v ^. tString
                             (a, b)   = v ^. textOptions ^. optalignment
                             fsze     = v ^. textOptions ^. optfontSize

-- # moveTo (p2 (x, y)) <> circle 1 # fc red
-- fslant   = v ^. textOptions ^. optfontSlant
-- fwght    = v ^. textOptions ^. optfontWeight
-- need to find a method to move text string to
-- a point in plot, translate and move doesnt work.
-- Implement a font slant and font weight.

  defLegendPic TextPlot {..} pp
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle pp

------------------------------------------------------------------------
-- Text plot
------------------------------------------------------------------------

-- | Draw a given string at a given point.
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

------------------------------------------------------------------------
-- Textplot
------------------------------------------------------------------------

textPlot
  :: (v ~ BaseSpace c,
      RealFloat n,
      Typeable n,
      PointLike v n (V2 n),
      MonadState (Axis b c n) m,
      Plotable (TextPlot n) b,
      v ~ V2)
  => (n,n) -> String -> m ()
textPlot pt a = addPlotable (mkTextPlot pt a)

textPlot'
  :: (v ~ BaseSpace c,
      RealFloat n,
      Typeable n,
      PointLike v n (V2 n),
      MonadState (Axis b c n) m,
      Plotable (TextPlot n) b,
      v ~ V2)
  => (n,n) -> String -> PlotState (TextPlot n) b -> m ()
textPlot' pt a = addPlotable' (mkTextPlot pt a)

textPlotL
  :: (v ~ BaseSpace c,
      RealFloat n,
      Typeable n,
      PointLike v n (V2 n),
      MonadState (Axis b c n) m,
      Plotable (TextPlot n) b,
      v ~ V2)
  => String -> (n,n) -> String -> m ()
textPlotL l pt a = addPlotableL l (mkTextPlot pt a)
