{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Plots.Name where

import Control.Lens
import Data.Map            (Map)
import Data.Ord            (comparing)
import Data.Typeable
import Diagrams.Core.Names
import Diagrams.Extra
import Diagrams.Prelude    hiding (view)

data PlotName = PlotName
  { _plotName    :: String
  , _namedSize2D :: SizeSpec2D
  , _namedT2     :: T2
  } deriving Typeable

makeLenses ''PlotName

instance Show PlotName where
  show pn = "Plot: " ++ view plotName pn

-- equating :: Eq b => (a -> b) -> a -> a -> Bool
-- equating = on (==)

instance Eq PlotName where
  (==) = equating (view plotName)

instance Ord PlotName where
  compare = comparing (view plotName)

instance IsName PlotName

_AName :: IsName a => Prism' AName a
_AName = prism' AName (\(AName a) -> cast a)

_Names :: IsName a => Traversal' Name a
_Names = _Wrapped' . traverse . _AName

_NamedString :: Traversal' Name String
_NamedString = _Names

_NamedPlot :: Traversal' Name PlotName
_NamedPlot = _Names

diaNames :: Diagram b R2 -> Map Name [P2]
diaNames = over (mapped . traversed) location . view (subMap . _Wrapped')

