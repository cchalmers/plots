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

data Title b v n = Title
  { tVisible   :: Bool
  , tTxt       :: String
  , tTxtFun    :: TextAlignment n -> String -> QDiagram b v n Any
  , tStyle     :: Style v n
  , tPlacement :: Placement
  , tGap       :: n
  } deriving Typeable

instance (Renderable (Text n) b, TypeableFloat n)
  => Default (Title b V2 n) where
  def = Title
    { tVisible = True
    , tTxt     = ""
    , tTxtFun  = mkText
    , tStyle   = mempty # fontSize (output 11)
    , tPlacement = midAbove
    , tGap = 20
    }

type instance V (Title b v n) = v
type instance N (Title b v n) = n

instance HasVisibility (Title b v n) where
  visible = lens tVisible (\t b -> t {tVisible = b})

instance HasGap (Title b v n) where
  gap = lens tGap (\t g -> t {tGap = g})

instance HasPlacement (Title b v n) where
  placement = titlePlacement

class HasTitle a b | a -> b where
  title :: Lens' a (Title b (V a) (N a))

  -- | The text used for the title. If the string is empty, no title is
  --   drawn.
  --
  --   Default is @""@
  titleText :: Lens' a String
  titleText = title . lens tTxt (\t s -> t {tTxt = s})

  -- | The style applied to the title.
  --
  --   Default is 'mempty'.
  titleStyle :: Lens' a (Style (V a) (N a))
  titleStyle = title . lens tStyle (\t s -> t {tStyle = s})

  -- | The placement of the title against the axis.
  --
  --   Default is 'mempty'.
  titlePlacement :: Lens' a Placement
  titlePlacement = title . lens tPlacement (\t s -> t {tPlacement = s})

  -- | The gap between the axis and the title.
  --
  --   Default is 'mempty'.
  titleGap :: Lens' a (N a)
  titleGap = title . lens tGap (\t s -> t {tGap = s})

instance HasTitle (Title b v n) b where
  title = id

-- | Render the title and place it around the bounding box.
drawTitle
  :: TypeableFloat n
  => BoundingBox V2 n
  -> Title b V2 n
  -> QDiagram b V2 n Any
drawTitle bb t
  | t ^. hidden || nullOf titleText t = mempty
  | otherwise = placeAgainst
                  bb
                  (t ^. titlePlacement)
                  (t ^. titleGap)
                  tDia
  where
    tDia = tTxtFun t (BoxAlignedText 0.5 0) (tTxt t)
             # applyStyle (tStyle t)

