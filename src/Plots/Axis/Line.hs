{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Line
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The lines that make up an axis.
--
----------------------------------------------------------------------------
module Plots.Axis.Line
  ( -- * Grid lines
    AxisLine
  , HasAxisLine (..)

    -- * Axis line types
  , AxisLineType (..)

  ) where

import           Control.Lens     hiding (( # ))
import           Data.Data
import           Data.Default

import           Diagrams.Prelude
import Plots.Types

-- | Where axis line for coordinate should be drawn. The 'Default' is
--   'BoxAxisLine'.
data AxisLineType
  = BoxAxisLine
  | LeftAxisLine
  | MiddleAxisLine
  | RightAxisLine
  | NoAxisLine
  deriving (Show, Eq, Typeable)

instance Default AxisLineType where
  def = BoxAxisLine

-- | Information about position and style of axis lines.
data AxisLine v n = AxisLine
  { alType      :: AxisLineType
  , alArrowOpts :: Maybe (ArrowOpts n)
  , alVisible   :: Bool
  , alStyle     :: Style v n
  } deriving Typeable

type instance V (AxisLine v n) = v
type instance N (AxisLine v n) = n

-- | Class of object that have an 'AxisLine'.
class HasAxisLine f a where
  -- | Lens onto the 'AxisLine'.
  axisLine :: LensLike' f a (AxisLine (V a) (N a))

  -- | The position of the axis line around the axis.
  --
  --   'Default' is 'BoxAxisLine'.
  axisLineType :: Functor f => LensLike' f a AxisLineType
  axisLineType = axisLine . lens alType (\al sty -> al {alType = sty})

  -- | The options for if you want the axis line to have arrows at the
  --   end.
  --
  --   'Default' is 'Nothing'.
  --
  --   XXX (feature not currently implimented)
  axisLineArrowOpts :: Functor f => LensLike' f a (Maybe (ArrowOpts (N a)))
  axisLineArrowOpts = axisLine . lens alArrowOpts (\al sty -> al {alArrowOpts = sty})

  -- | The 'Style' applied to the axis line
  axisLineStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  axisLineStyle = axisLine . lens alStyle (\al sty -> al {alStyle = sty})

instance HasAxisLine f (AxisLine v n) where
  axisLine = id

--   Note this is different from 'NoAxisLine'. Other parts that are
--   tied to the axis line will still be present when
--   'axisLineVisible' is 'False'. But if 'NoAxisLine' is set, there
--   never any line for those things to attach to, so they don't
--   exist.
instance HasVisibility (AxisLine v n) where
  visible = lens alVisible (\al b -> al {alVisible = b})

instance Typeable n => Default (AxisLine v n) where
  def = AxisLine
    { alType  = def
    , alArrowOpts = def
    , alVisible = True
    , alStyle = mempty
    }

