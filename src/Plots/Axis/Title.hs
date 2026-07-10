{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Title
-- Copyright   :  (C) 2016 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The title used for a plot.
--
----------------------------------------------------------------------------
module Plots.Axis.Title
  ( Title
  , HasTitle (..)
  , drawTitle
  ) where

import Data.Default
import Data.Typeable

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Plots.Types

data Title v = Title
  { tVisible   :: Bool
  , tTxt       :: String
  , tTxtFun    :: TextAlignment Double -> String -> Diagram v
  , tStyle     :: Style v Double
  , tPlacement :: Placement
  , tAlignment :: TextAlignment Double
  , tGap       :: Double
  } deriving Typeable

instance Default (Title V2) where
  def = Title
    { tVisible = True
    , tTxt     = ""
    , tTxtFun  = mkText
    , tStyle   = mempty # fontSize (output 11)
    , tPlacement = midAbove
    , tAlignment = BoxAlignedText 0.5 0
    , tGap = 20
    }

instance Default (Title V3) where
  def = Title
    { tVisible = True
    , tTxt     = ""
    , tTxtFun  = mempty
    , tStyle   = mempty # fontSize (output 11)
    , tPlacement = midAbove
    , tAlignment = BoxAlignedText 0.5 0
    , tGap = 20
    }

type instance V (Title v) = v
type instance N (Title v) = Double

instance HasVisibility (Title v) where
  visible = lens tVisible (\t b -> t {tVisible = b})

instance HasGap (Title v) where
  gap = lens tGap (\t g -> t {tGap = g})

instance HasPlacement (Title v) where
  placement = titlePlacement

class HasTitle a where
  title :: Lens' a (Title (V a))

  -- | The text used for the title. If the string is empty, no title is
  --   drawn.
  --
  --   Default is @""@
  titleText :: Lens' a String
  titleText = title . lens tTxt (\t s -> t {tTxt = s})

  -- | The style applied to the title.
  --
  --   Default is 'mempty'.
  titleStyle :: Lens' a (Style (V a) Double)
  titleStyle = title . lens tStyle (\t s -> t {tStyle = s})

  -- | The placement of the title against the axis.
  --
  --   Default is 'mempty'.
  titlePlacement :: Lens' a Placement
  titlePlacement = title . lens tPlacement (\t s -> t {tPlacement = s})

  -- | The function used to draw the title text.
  --
  --   Default is 'mkText'.
  titleTextFunction :: Lens' a (TextAlignment Double -> String -> Diagram (V a))
  titleTextFunction = title . lens tTxtFun (\t s -> t {tTxtFun = s})

  -- | The 'TextAlignment' used for the title text. This is given to the
  --   'titleTextFunction'.
  --
  --   Default is @'BoxAlignedText' 0.5 0@.
  titleAlignment :: Lens' a (TextAlignment Double)
  titleAlignment = title . lens tAlignment (\t s -> t {tAlignment = s})

  -- | The gap between the axis and the title.
  --
  --   Default is 'mempty'.
  titleGap :: Lens' a Double
  titleGap = title . lens tGap (\t s -> t {tGap = s})

instance HasTitle (Title v) where
  title = id

-- | Render the title and place it around the bounding box.
drawTitle
  :: BoundingBox V2 Double
  -> Title V2
  -> Diagram V2
drawTitle bb t
  | t ^. hidden || nullOf titleText t = mempty
  | otherwise = placeAgainst
                  bb
                  (t ^. titlePlacement)
                  (t ^. titleGap)
                  tDia
  where
    tDia = tTxtFun t (t ^. titleAlignment) (tTxt t)
             # applyStyle (tStyle t)

