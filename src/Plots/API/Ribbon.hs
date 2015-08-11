{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Ribbon
  ( ribbonPlot
  , ribbonPlot'
  , ribbonPlotL
  , ribbonPlotOf
  , ribbonPlotOf'
  , ribbonPlotLOf

  , makeribbon
  , makearea
  , makearea'
  , makeareagroup'

  , barPlot
  , barPlot'
  , barPlotL

  , barPlotNormal
  , barPlotNormalC
  , barPlotNormalCL
  , barPlotNormalMulti
  , barPlotNormalMultiC

  , barPlotStacked
  , barPlotStackedC
  , barPlotStackedCL
  , barPlotStackedMultiC

  , barPlotSplit
  , barPlotSplitC
  , barPlotSplitCL
  , barPlotSplitMultiC

  , barPlotRatio
  , barPlotRatioC
  , barPlotRatioCL
  , barPlotRatioMultiC
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable
import qualified Data.Foldable as F
import           Data.List
import           Data.Function

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Linear

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes

import           Plots.Types.Ribbon
import           Plots.API

------------------------------------------------------------------------
-- Ribbon
------------------------------------------------------------------------
ribbonPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b,
      F.Foldable f)
  => f p -> m ()
ribbonPlot a = addPlotable (mkRibbonPlot a)

ribbonPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b,
      F.Foldable f)
  => f p -> PlotState (RibbonPlot v n) b -> m ()
ribbonPlot' d = addPlotable' (mkRibbonPlot d)

ribbonPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
ribbonPlotL l d = addPlotableL l (mkRibbonPlot d)

-- Fold variants

ribbonPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b)
  => Fold s p -> s -> m ()
ribbonPlotOf f s = addPlotable (mkRibbonPlotOf f s)

ribbonPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b)
  => Fold s p -> s -> PlotState (RibbonPlot v n) b -> m ()
ribbonPlotOf' f s = addPlotable' (mkRibbonPlotOf f s)

ribbonPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b)
  => String -> Fold s p -> s -> m ()
ribbonPlotLOf l f s = addPlotableL l (mkRibbonPlotOf f s)

------------------------------------------------------------------------
-- Area
------------------------------------------------------------------------

makeribbon :: (RealFloat b, Typeable b, Typeable b1, Renderable (Path V2 b) b1,
               MonadState (Axis b1 c b) m, BaseSpace c ~ V2) =>
              [b] -> [b] -> [b] -> Colour Double -> m ()
makeribbon x1s x2s ys colour = ribbonPlot' ((zip x1s ys) ++ reverse (zip x2s ys)) $ do
                                 addLegendEntry "ribbon test"
                                 plotColor .= colour

makearea :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            [Double] -> [Double] -> Colour Double -> m ()
makearea xs ys colour = ribbonPlot' ((zip xs ys) ++ reverse (zeroY (zip xs ys))) $ do
                                 addLegendEntry "ribbon test"
                                 plotColor .= colour

makearea' :: (Typeable b, Renderable (Path V2 Double) b,
              MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
             [(Double, Double)] -> String -> m ()
makearea' sd string = ribbonPlot' (sd ++ reverse (zeroY sd)) $ do
                                 addLegendEntry string

-- makeareagroup xs ys ds = makeareagroup' (creategroupdata (zip xs ys) ds)
-- error when using group

makeareagroup' :: (Typeable b, Renderable (Path V2 Double) b,
                   MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  m ([(Double, Double)], String) -> m ()
makeareagroup' xs  = do x <- xs
                        makearea' (fst x) (snd x)

------------------------------------------------------------------------
-- Bar
------------------------------------------------------------------------

barPlot :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> m ()
barPlot x w = ribbonPlot (createBarData x w)

barPlot' :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> PlotState (RibbonPlot V2 Double) b -> m ()
barPlot' x w = ribbonPlot' (createBarData x w)

barPlotC :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> Colour Double -> m ()
barPlotC x w colour = ribbonPlot' (createBarData x w) $ do
                                plotColor .= colour

barPlotL :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> String -> Colour Double -> m ()
barPlotL x w string colour = ribbonPlot' (createBarData x w) $ do
                                plotColor .= colour
                                addLegendEntry string

------------------------------------------------------------------------
-- Normal Bar --figure out a way to loop and use sort
------------------------------------------------------------------------
barPlotNormal :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotNormal y xs w = F.for_ xs $ \x -> barPlot ((fromIntegral y), x) w


barPlotNormalC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]-> Double -> m ()
barPlotNormalC y xs cs w = F.for_ (sortBy (compare `on` fst)(zip xs cs)) $ \x -> barPlotC ((fromIntegral y), fst x) w (snd x) 


barPlotNormalCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] -> [String]-> Double -> m ()
barPlotNormalCL y xs cs ls w = F.for_ (sortBy (compare `on` fstof3)(zip3 xs cs ls)) $ \x -> barPlotL ((fromIntegral y), fstof3 x) w (trdof3 x) (sndof3 x) 

fstof3 (a, _, _) = a
sndof3 (_, a, _) = a
trdof3 (_, _, a) = a

barPlotNormalMulti :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> Double -> m ()
barPlotNormalMulti xs w = F.for_ [1 .. length xs] $ \x -> barPlotNormal x (xs!!(x-1)) w 

barPlotNormalMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double] -> [String] -> Double -> m ()
barPlotNormalMultiC xs colormap names w = do 
                                          barPlotNormalCL 1 (xs!!0) colormap names w
                                          F.for_ [2 .. length xs] $ \x -> barPlotNormalC x (xs!!(x-1)) colormap w 

------------------------------------------------------------------------
-- Stacked Bar
------------------------------------------------------------------------
barPlotStacked :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotStacked y xs w = F.for_ (additive xs) $ \x -> barPlot ((fromIntegral y), x) w

additive :: [Double] -> [Double]
additive [] = []
additive (x:[]) = [x]
additive (x:xs) = x:(additive (addtofst x xs))

addtofst :: Double -> [Double] -> [Double]
addtofst a (x:xs) = (a+x):xs

barPlotStackedC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]-> Double -> m ()
barPlotStackedC y xs cs w = F.for_ (sortBy (compare `on` fst)(zip (additive xs) cs)) $ \x -> barPlotC ((fromIntegral y), fst x) w (snd x) 

barPlotStackedCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]
                     -> [String] -> Double -> m ()
barPlotStackedCL y xs cs ls w = F.for_ (sortBy (compare `on` fstof3)(zip3 (additive xs) cs ls)) $ \x -> barPlotL ((fromIntegral y), fstof3 x) w (trdof3 x) (sndof3 x)

barPlotStackedMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double]-> [String] -> Double -> m ()
barPlotStackedMultiC xs colormap names w = do 
                                          barPlotStackedCL 1 (xs!!0) colormap names w
                                          F.for_ [2 .. length xs] $ \x -> barPlotStackedC x (xs!!(x-1)) colormap w 

------------------------------------------------------------------------
-- Split Bar
------------------------------------------------------------------------
barPlotSplit :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotSplit y xs w = F.for_ (zip (createsplitdata y len w) xs) $ \x -> barPlot x w1
                      where w1 = w/fromIntegral len
                            len = length xs

createsplitdata :: Int -> Int -> Double -> [Double]
createsplitdata y len w =  [b, b+w1 .. (a-w1)]
                           where y1 = fromIntegral y
                                 w1 = w/fromIntegral len
                                 a  = y1+(w/2)+ (w1/2)
                                 b  = y1-(w/2)+ (w1/2)

barPlotSplitC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]-> Double -> m ()
barPlotSplitC y xs cs w = F.for_ (zip (zip (createsplitdata y len w) xs) cs) $ \x -> barPlotC (fst x) w1 (snd x) 
                         where w1  = w/fromIntegral len
                               len = length xs

barPlotSplitCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] 
                     -> [String] -> Double -> m ()
barPlotSplitCL y xs cs ls w = F.for_ (zip3 (zip (createsplitdata y len w) xs) cs ls) $ \x -> barPlotL (fstof3 x) w1 (trdof3 x) (sndof3 x) 
                             where w1  = w/fromIntegral len
                                   len = length xs

barPlotSplitMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double]-> [String] -> Double -> m ()
barPlotSplitMultiC xs colormap names w = do 
                                         barPlotSplitCL 1 (xs!!0) colormap names w
                                         F.for_ [2 .. length xs] $ \x -> barPlotSplitC x (xs!!(x-1)) colormap w 
------------------------------------------------------------------------
-- Ratio Bar
------------------------------------------------------------------------
barPlotRatio :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotRatio t xs w = do 
                      barPlotStacked t [ x/tot | x <- xs] w
                      where tot = sum xs

barPlotRatioC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] -> Double -> m ()
barPlotRatioC t xs colormap w = do 
                                barPlotStackedC t [x/tot | x <- xs] colormap w
                                where tot = sum xs
                             
barPlotRatioCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] 
                     -> [String]-> Double -> m ()
barPlotRatioCL t xs colormap names w = do 
                                       barPlotStackedCL t [x/tot | x <- xs] colormap names w
                                       where tot = sum xs

barPlotRatioMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double] -> [String] -> Double -> m ()
barPlotRatioMultiC xs colormap names w = do 
                                         barPlotRatioCL 1 (xs!!0) colormap names w
                                         F.for_ [2 .. length xs] $ \x -> barPlotRatioC x (xs!!(x-1)) colormap w 
