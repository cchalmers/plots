{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Plots.Name where

import Control.Lens
import Data.Map            (Map)
import Data.Ord            (comparing)
import Data.Function
import Data.Typeable
import Diagrams.Core.Names
-- import Diagrams.Extra
import Diagrams.Prelude    hiding (view)

data PlotName n = PlotName
  { _plotName    :: String
  , _namedSize2D :: SizeSpec V2 n
  , _namedT2     :: T2 n
  } deriving Typeable

makeLenses ''PlotName

instance Show (PlotName n) where
  show pn = "Plot: " ++ view plotName pn

-- equating :: Eq b => (a -> b) -> a -> a -> Bool
-- equating = on (==)

instance Eq (PlotName n) where
  (==) = on (==) (view plotName)

instance Ord (PlotName n) where
  compare = comparing (view plotName)

instance Typeable n => IsName (PlotName n)

_AName :: IsName a => Prism' AName a
_AName = prism' AName (\(AName a) -> cast a)

_Names :: IsName a => Traversal' Name a
_Names = _Wrapped' . traverse . _AName

_NamedString :: Traversal' Name String
_NamedString = _Names

_NamedPlot :: Typeable n => Traversal' Name (PlotName n)
_NamedPlot = _Names

diaNames :: OrderedField n => QDiagram b V2 n Any -> Map Name [P2 n]
diaNames = over (mapped . traversed) location . view (subMap . _Wrapped')

