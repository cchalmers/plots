{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.HeatMap
-- Copyright   :  (C) 2016 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A heat map is a graphical representation of data where the individual
-- values contained in a matrix are represented as colours.
--
-- <<diagrams/src_Plots_Types_HeatMap_heatMapIndexedExample.svg#diagram=heatMapIndexedExample&height=350>>
--
-- (see 'heatMapIndexed' example for code to make this plot)
--
----------------------------------------------------------------------------

module Plots.Types.HeatMap
  ( -- * Heat map
    HeatMap
  , heatMap
  , heatMap'
  , heatMapIndexed
  , heatMapIndexed'

  -- * Lenses
  , HasHeatMap (..)

  -- ** Rendering functions
  , pathHeatRender
  , pixelHeatRender
  , pixelHeatRender'

  -- * Heat matrix
  , HeatMatrix (..)
  , heatImage
  , hmPoints
  , hmSize

  -- * Low level construction
  , mkHeatMap
  , mkHeatSurface
  , mkHeatMatrix
  , mkHeatMatrix'

  ) where

import           Control.Lens                    hiding (transform, ( # ))

import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.Foldable                   as F
import           Data.Typeable
import qualified Data.Vector.Generic.Mutable     as M
import qualified Data.Vector.Storable            as S
import qualified Data.Vector.Unboxed             as V
import           Data.Word                       (Word8)

import           Codec.Picture
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Style
import           Plots.Types

import Diagrams.TwoD.Image

import Geometry.ThreeD.Shapes

------------------------------------------------------------------------
-- HeatMap
------------------------------------------------------------------------

-- | 2D Array of 'Double's.
data HeatMatrix = HeatMatrix
  { hmSize       :: {-# UNPACK #-} !(V2 Int)
    -- ^ The size of heat matrix.
  , _hmVector    :: {-# UNPACK #-} !(V.Vector Double)
  , hmBoundLower :: {-# UNPACK #-} !Double
  , hmBoundUpper :: {-# UNPACK #-} !Double
  }

-- | Construct a heat matrix from a size and a generating function.
mkHeatMatrix :: V2 Int -> (V2 Int -> Double) -> HeatMatrix
mkHeatMatrix s@(V2 x y) f = runST $ do
  mv <- M.new (x*y)

  let go !q !a !b !i !j
        | j == y    = do v <- V.unsafeFreeze mv
                         return (HeatMatrix s v a b)
        | i == x    = go q a b 0 (j + 1)
        | otherwise = do let !d = f (V2 i j)
                         M.unsafeWrite mv q d
                         go (q + 1) (min a d) (max b d) (i + 1) j

  go 0 (1/0) (-1/0) 0 0
{-# INLINE mkHeatMatrix #-}

min' :: Double -> Double -> Double
min' !a !b
  | isNaN b      = a
  | isInfinite b = a
  | otherwise    = min a b
{-# INLINE min' #-}

max' :: Double -> Double -> Double
max' !a !b
  | isNaN b      = a
  | isInfinite b = a
  | otherwise    = max a b
{-# INLINE max' #-}

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of a vector in one pass. This
--   ignores any @Infinity@ or @NaN@ values (since these make no sense
--   for colour bar ranges).
minMax :: V.Vector Double -> (Double, Double)
minMax = fini . V.foldl' go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min' lo k) (max' hi k)
    fini (MM lo hi) = (lo, hi)
{-# INLINE minMax #-}

-- | Construct a heat matrix from a foldable of foldables.
--
-- @
-- 'mkHeatMatrix'' :: [['Double']] -> 'HeatMatrix'
-- 'mkHeatMatrix'' :: ['Vector' 'Double'] -> 'HeatMatrix'
-- @
mkHeatMatrix' :: (F.Foldable f, F.Foldable g) => f (g Double) -> HeatMatrix
mkHeatMatrix' xss = HeatMatrix (V2 x y) vd a b
  where
  (a,b) = minMax vd
  vd = V.create $ do
    mv <- M.new (x*y)
    let go !_ []     = return mv
        go  j (r:rs) = V.unsafeCopy (M.unsafeSlice (j*x) x mv) r >> go (j-1) rs
    go (y - 1) vs
  -- vs is in reverse since we used foldl' to build it
  (!x,!y,!vs) = F.foldl' f (maxBound,0,[]) xss
  f (!i,!j,!ss) xs = let !v = V.fromList (F.toList xs)
                     in  (min i (V.length v), j+1, v : ss)

-- | Indexed traversal over the values of a 'HeatMatrix'.
hmPoints :: IndexedTraversal' (V2 Int) HeatMatrix Double
hmPoints f (HeatMatrix e@(V2 x y) v a b) =
  go 0 0 0 <&> \vs ->
    let v'= V.fromListN (x*y) vs
    in  HeatMatrix e v' a b
  where
    -- V2 x y = hmExtent
    go !s !i !j
      | i >= x    = go s 0 (j+1)
      | j >= y    = pure []
      | otherwise = (:) <$> indexed f (V2 i j) (V.unsafeIndex v s)
                        <*> go (s+1) (i+1) j
{-# INLINE [0] hmPoints #-}

{-# RULES
  "hmPoints/foldr"
  hmPoints = ifoldring hmFold :: Getting (Endo r) HeatMatrix Double;
  "hmPoints/ifoldr"
  hmPoints = ifoldring hmFold :: IndexedGetting (V2 Int) (Endo r) HeatMatrix Double
 #-}

hmFold :: (V2 Int -> Double -> b -> b) -> b -> HeatMatrix -> b
hmFold f b0 (HeatMatrix (V2 x y) v _ _) = go 0 0 0 b0 where
  go !s !i !j b
    | i >= x    = go s 0 (j+1) b
    | j >= y    = b
    | otherwise = f (V2 i j) (V.unsafeIndex v s) (go (s+1) (i+1) j b)
{-# INLINE hmFold #-}

-- Rendering heat matrices --------------------------------------------

-- | Render an heatmap as an 'ImageRGB8'.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_pixelHeatRenderExample.svg#diagram=pixelHeatRenderExample&height=350>>
--
-- > import Plots
-- >
-- > pixelHeatRenderExample =
-- >   let f (V2 x y) = fromIntegral x + fromIntegral y
-- >       myHM       = mkHeatMatrix (V2 5 5) f
-- >   in  pixelHeatRender myHM viridis
--
pixelHeatRender :: HeatMatrix -> ColourMap -> Diagram V2
pixelHeatRender hm cm =
  alignBL . image $ DImage x y (ImageRaster (ImageRGB8 img))
  where
    img    = heatImage hm cm
    V2 x y = hmSize hm

-- | Render an heatmap as an 'ImageRGB8' with @n@ pixels per heat matrix
--   point.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_pixelHeatRenderExample'.svg#diagram=pixelHeatRenderExample'&height=350>>
--
-- > import Plots
-- >
-- > pixelHeatRenderExample' =
-- >   let f (V2 x y) = fromIntegral x + fromIntegral y
-- >       myHM       = mkHeatMatrix (V2 5 5) f
-- >   in  pixelHeatRender' 10 myHM viridis
--
pixelHeatRender' :: Int -> HeatMatrix -> ColourMap -> Diagram V2
pixelHeatRender' n hm cm =
  scale (1/fromIntegral n) . alignBL . image $ DImage (x*n) (y*n) (ImageRaster (ImageRGB8 img))
  where
    img    = scaleImage n $ heatImage hm cm
    V2 x y = hmSize hm

-- | Scale an image so each pixel takes (n*n) pixels. This can be
--   useful for using 'heatImage' on small heat matrices to give a
--   sharper image.
scaleImage :: Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImage n img | n == 1  = img
                 | n == 0  = Image 0 0 S.empty
                 | n <  0  = error "scaleImage: negative scale"
scaleImage n (Image x y v) = Image (n*x) (n*y) vn where
  !refV = V.fromList $ map (*3) [ i + n*x*j | i <- [0..n-1], j <- [0..n-1] ]
  !n3 = 3*n
  vn = S.create $ do
    mv <- M.new (n * n * S.length v)
    let go !q !i !s | q >= 3*x*y = return mv
                    | i == x     = go q 0 (s + 3*x*n*(n-1))
        go  q  i  s              = do
          let !r = S.unsafeIndex v  q
              !g = S.unsafeIndex v (q+1)
              !b = S.unsafeIndex v (q+2)
          V.forM_ refV $ \ds -> do
            M.unsafeWrite mv (s + ds    ) r
            M.unsafeWrite mv (s + ds + 1) g
            M.unsafeWrite mv (s + ds + 2) b
          go (q+3) (i+1) (s+n3)

    go 0 0 0

-- | Create an image of 'PixelsRGB8' using the heat matrix.
heatImage :: HeatMatrix -> ColourMap -> Image PixelRGB8
heatImage (HeatMatrix (V2 x y) dv a b) cm = Image x y v where
  !cv = mkColourVector cm

  v :: S.Vector Word8
  v = S.create $ do
    mv <- M.new (3 * x * y)
    let !m = 256 / (b - a)
        writeColour q (toSRGB24 -> RGB rr gg bb) = do
          M.unsafeWrite mv  q    rr
          M.unsafeWrite mv (q+1) gg
          M.unsafeWrite mv (q+2) bb

        colourValue !q !d
          | isNaN d      = writeColour q (cm^.nanColour)
          | isInfinite d = writeColour q (if d < 0 then cm^.negInfColour else cm^.infColour)
          | otherwise    = do
              let !o = 3 * (min 255 . max 0 . round $ (d - a) * m)
              M.unsafeWrite mv  q    (V.unsafeIndex cv  o   )
              M.unsafeWrite mv (q+1) (V.unsafeIndex cv (o+1))
              M.unsafeWrite mv (q+2) (V.unsafeIndex cv (o+2))

        go s i q
          | s == x-1  = do
              let d = V.unsafeIndex dv s
              colourValue q d
              return mv
          | i == x    = go (s - 2*x) 0 q
          | otherwise = do
              let d = V.unsafeIndex dv s
              colourValue q d
              go (s+1) (i+1) (q+3)
    go (x * (y-1)) 0 0

-- Make an unboxed colour map using 256 samples.
mkColourVector :: ColourMap -> V.Vector Word8
mkColourVector cm = V.create $ do
  mv <- M.new (3*256)

  let go i | i == 3*256 = return mv
           | otherwise  = do
               let x = fromIntegral i / (3*256)
                   RGB r g b = cm ^. ixColourR x . to toSRGB24
               M.unsafeWrite mv  i    r
               M.unsafeWrite mv (i+1) g
               M.unsafeWrite mv (i+2) b
               go (i+3)

  go 0

-- | Render the heat map as a collection squares made up of 'Trail's.
--   This method is compatible with all backends and should always look
--   sharp. However it can become slow and large for large heat maps.
--
--   It is recommended to use 'pathHeatRender' for small heat maps and
--   'pixelHeatRender' for larger ones.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_pathHeatRenderExample.svg#diagram=pathHeatRenderExample&height=350>>
--
-- > import Plots
-- >
-- > pathHeatRenderExample =
-- >   let f (V2 x y) = fromIntegral x + fromIntegral y
-- >       myHM       = mkHeatMatrix (V2 5 5) f
-- >   in  pathHeatRender myHM viridis
--
pathHeatRender :: HeatMatrix -> ColourMap -> Diagram V2
pathHeatRender hm@(HeatMatrix _ _ a b) cm = ifoldMapOf hmPoints mk hm # lwO 0
  where
    normalise d = (d - a) / (b - a)
    mk v@(V2 i j) d =
      rect w h
        # alignTR
        # translate (fromIntegral <$> v ^+^ 1)
        # fc (cm ^. ixColour (normalise d))
      where
        -- Squares that are not on the top left edge are slightly
        -- bigger to remove phantom gaps
        w | i == 0    = 1
          | otherwise = 1.5
        h | j == 0    = 1
          | otherwise = 1.5

------------------------------------------------------------------------
-- Heat matrix
------------------------------------------------------------------------

-- | A mapping from points in a 2D axis do 'Double's. These 'Double's
--   are converted to colours using the axis 'ColourMap'.
data HeatMap v = HeatMap
  { hMatrix      :: HeatMatrix
  , hStart       :: P2 Double
  , hSize        :: V2 Double
  , hGridSty     :: Style v Double
  , hGridVisible :: Bool
  , hLimits      :: Maybe (Double,Double)
  , hDraw        :: HeatMatrix -> ColourMap -> Diagram v
  } deriving Typeable

type instance V (HeatMap v) = v
type instance N (HeatMap v) = Double

-- | Class of things that let you change the heatmap options.
class HasHeatMap f a where
  -- | Lens onto the heatmap options.
  heatMapOptions :: LensLike' f a (HeatMap (V a))

  -- | Whether there should be grid lines draw for the heat map.
  --
  --   Default is 'False'.
  heatMapGridVisible :: Functor f => LensLike' f a Bool
  heatMapGridVisible = heatMapOptions . lens hGridVisible (\s b -> (s {hGridVisible = b}))

  -- | The style applied to the grid lines for the heat map, if they're
  --   visible.
  --
  --   Default is 'mempty'.
  heatMapGridStyle :: Functor f => LensLike' f a (Style (V a) Double)
  heatMapGridStyle = heatMapOptions . lens hGridSty (\s b -> (s {hGridSty = b}))

  -- | The size of each individual square in the heat map.
  --
  --   Default is @'V2' 1 1@.
  heatMapSize :: Functor f => LensLike' f a (V2 Double)
  heatMapSize = heatMapOptions . lens hSize (\s b -> (s {hSize = b}))

  -- | The size of the full extent of the heat map.
  --
  --   Default is extent of the heat matrix.
  heatMapExtent :: Functor f => LensLike' f a (V2 Double)
  heatMapExtent = heatMapOptions . l where
    l f hm = f (hSize hm * s) <&> \x -> hm { hSize = x / s }
      where s = fmap fromIntegral (hmSize $ hMatrix hm)

  -- | The starting point at the bottom left corner of the heat map.
  --
  --   Default is 'origin'
  heatMapStart :: Functor f => LensLike' f a (P2 Double)
  heatMapStart = heatMapOptions . lens hStart (\s b -> (s {hStart = b}))

  -- | The center point of the heat map.
  heatMapCentre :: Functor f => LensLike' f a (P2 Double)
  heatMapCentre = heatMapOptions . l where
    l f hm = f (hStart hm .+^ v) <&> \p -> hm { hStart = p .-^ v }
      where v = fmap fromIntegral (hmSize $ hMatrix hm) * hSize hm / 2

  -- | Limits @(a,b)@ used on the data such that @a@ is the start of the
  --   'ColourMap' and @b@ is the end of the 'ColourMap'. Default is @(0,1)@.
  heatMapLimits :: Functor f => LensLike' f a (Maybe (Double, Double))
  heatMapLimits = heatMapOptions . lens hLimits (\s b -> (s {hLimits = b}))

  -- | Funtion used to render the heat map. See 'pathHeatRender' and
  --   'pixelHeatRender'.
  --
  --   Default is 'pathHeatRender'.
  heatMapRender :: Functor f => LensLike' f a (HeatMatrix -> ColourMap -> Diagram (V a))
  heatMapRender = heatMapOptions . lens hDraw (\s b -> (s {hDraw = b}))

instance HasHeatMap f (HeatMap v) where
  heatMapOptions = id

instance (Functor f, HasHeatMap f a) => HasHeatMap f (Plot a) where
  heatMapOptions = rawPlot . heatMapOptions

instance Enveloped (HeatMap V2) where
  getEnvelope hm = getEnvelope (fromCorners p (p .+^ v))
    where p = view heatMapStart hm
          v = view heatMapExtent hm

instance Enveloped (HeatMap V3) where
  getEnvelope hm = getEnvelope (fromCorners (v3 p) (v3 $ p .+^ v))
    where p = view heatMapStart hm
          v = view heatMapExtent hm
          v3 (P (V2 x y)) = P $ V3 x y 0

instance Plotable (HeatMap V2) where
  renderPlotable s _sty HeatMap {..} =
      transform (s^.specTrans) $
        grid <> hDraw matrix' (s^.specColourMap)
                  # scaleV hSize
                  # moveTo hStart
    where
      --- TODO
      grid = mempty

      --- XXX need to give _range to the axis somehow (for colour bar range)
      matrix' = case hLimits of
        -- Just r@(a,b) -> (r, hMatrix { hmFun = (/ (b - a)) . (+a) . hmFun hMatrix })
        -- Nothing      -> normaliseHeatMatrix hMatrix
        Just (a,b) -> hMatrix { hmBoundLower = a, hmBoundUpper = b }
        Nothing     -> hMatrix

  -- XXX make better
  defLegendPic sty HeatMap {..} = square 5 # applyAreaStyle sty

instance Plotable (HeatMap V3) where
  renderPlotable s _sty HeatMap {..} =
      transform (s^.specTrans) $
        grid <> hDraw matrix' (s^.specColourMap)
                  # scaleV (v3 hSize)
                  # moveTo (p3 hStart)
    where
      v3 (V2 x y) = V3 x y 1
      p3 (P (V2 x y)) = P $ V3 x y 0
      --- TODO
      grid = mempty

      --- XXX need to give _range to the axis somehow (for colour bar range)
      matrix' = case hLimits of
        -- Just r@(a,b) -> (r, hMatrix { hmFun = (/ (b - a)) . (+a) . hmFun hMatrix })
        -- Nothing      -> normaliseHeatMatrix hMatrix
        Just (a,b) -> hMatrix { hmBoundLower = a, hmBoundUpper = b }
        Nothing    -> hMatrix

  -- XXX make better
  defLegendPic sty HeatMap {..} = cube # scale 5 # applyAreaStyle sty

-- | Construct a 'HeatMap' using the given 'HeatMatrix'.
mkHeatMap :: HeatMatrix -> HeatMap V2
mkHeatMap mat = HeatMap
  { hMatrix      = mat
  , hStart       = origin
  , hSize        = V2 1 1
  , hGridSty     = mempty
  , hGridVisible = False
  , hLimits      = Nothing
  , hDraw        = pathHeatRender
  }

mkHeatSurface :: HeatMatrix -> HeatMap V3
mkHeatSurface mat = HeatMap
  { hMatrix      = mat
  , hStart       = origin
  , hSize        = V2 1 1
  , hGridSty     = mempty
  , hGridVisible = False
  , hLimits      = Nothing
  , hDraw        = undefined
  }

-- Adding to axis ------------------------------------------------------

-- | Add a 'HeatMap' plot using the extent of the heatmap and a
--   generating function.
--
-- @
-- 'heatMap' :: [['Double']] -> 'State' ('Plot' ('HeatMap' b n)) () -> 'State' ('Axis' b 'V2' n) ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_heatMapExample.svg#diagram=heatMapExample&height=350>>
--
-- > import Plots
-- > heatMapAxis :: Axis B V2 Double
-- > heatMapAxis = r2Axis &~ do
-- >   display colourBar
-- >   axisExtend .= noExtend
-- >
-- >   let xs = [[1,2,3],[4,5,6]]
-- >   heatMap xs $ heatMapSize .= V2 10 10
--
-- > heatMapExample = renderAxis heatMapAxis
--
heatMap
  :: (F.Foldable f,
      F.Foldable g,
      MonadState (Axis V2) m)
  => f (g Double)
  -> State (Plot (HeatMap V2)) ()
                   -- ^ changes to plot options
  -> m ()          -- ^ add plot to 'Axis'
heatMap xss s = do
  let hm@(HeatMatrix _ _ a b) = mkHeatMatrix' xss
  addPlotable (mkHeatMap hm) s

  -- (don't like this way of doing it)
  colourBarRange .= over both realToFrac (a,b)

heatSurface
  :: (F.Foldable f,
      F.Foldable g,
      MonadState (Axis V3) m)
  => f (g Double)
  -> State (Plot (HeatMap V3)) ()
                   -- ^ changes to plot options
  -> m ()          -- ^ add plot to 'Axis'
heatSurface xss s = do
  let hm@(HeatMatrix _ _ a b) = mkHeatMatrix' xss
  addPlotable (mkHeatSurface hm) s

  -- (don't like this way of doing it)
  colourBarRange .= over both realToFrac (a,b)

-- | Add a 'HeatMap' plot using the extent of the heatmap and a
--   generating function.
--
-- @
-- 'heatMap'' :: [['Double']] -> 'State' ('Axis' b 'V2' n) ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_heatMapExample'.svg#diagram=heatMapExample'&height=350>>
--
-- > import Plots
-- > heatMapAxis' :: Axis B V2 Double
-- > heatMapAxis' = r2Axis &~ do
-- >   display colourBar
-- >   axisExtend .= noExtend
-- >   axisColourMap .= Plots.magma
-- >
-- >   let xs = [[1,2,3],[4,5,6]]
-- >   heatMap' xs
--
-- > heatMapExample' = renderAxis heatMapAxis'
--
heatMap'
  :: (F.Foldable f,
      F.Foldable g,
      MonadState (Axis V2) m)
  => f (g Double)
  -> m ()          -- ^ add plot to 'Axis'
heatMap' xss = heatMap xss (return ())

-- | Add a 'HeatMap' plot using the extent of the heatmap and a
--   generating function.
--
-- @
-- 'heatMapIndexed' :: 'V2' 'Int'     -> ('V2' 'Int' -> 'Double')     -> 'State' ('Plot' ('HeatMap' b n)) () -> 'State' ('Axis' b 'V2' n) ()
-- 'heatMapIndexed' :: ('Int', 'Int') -> (('Int', 'Int') -> 'Double') -> 'State' ('Plot' ('HeatMap' b n)) () -> 'State' ('Axis' b 'V2' n) ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_heatMapIndexedExample.svg#diagram=heatMapIndexedExample&height=350>>
--
-- > import Plots
-- > heatMapIndexedAxis :: Axis B V2 Double
-- > heatMapIndexedAxis = r2Axis &~ do
-- >   display colourBar
-- >   axisExtend .= noExtend
-- >
-- >   let f (V2 x y) = fromIntegral x + fromIntegral y
-- >   heatMapIndexed (V2 3 3) f $ heatMapSize .= V2 10 10
--
-- > heatMapIndexedExample = renderAxis heatMapIndexedAxis
--
heatMapIndexed
  :: (VectorLike V2 Int i,
      MonadState (Axis V2) m)
  => i             -- ^ extent of array
  -> (i -> Double) -- ^ heat from index
  -> State (Plot (HeatMap V2)) ()
                   -- ^ changes to plot options
  -> m ()          -- ^ add plot to 'Axis'
heatMapIndexed i f s = do
  let hm@(HeatMatrix _ _ a b) = mkHeatMatrix (view unvectorLike i) (f . view vectorLike)
  addPlotable (mkHeatMap hm) s

  -- (don't like this way of doing it)
  colourBarRange .= over both realToFrac (a,b)

-- | Add a 'HeatMap' plot using the extent of the heatmap and a
-- generating function without changes to the heap map options.
--
-- @
-- 'heatMapIndexed' :: 'V2' 'Int'     -> ('V2' 'Int' -> 'Double')     -> 'State' ('Axis' b 'V2' n) ()
-- 'heatMapIndexed' :: ('Int', 'Int') -> (('Int', 'Int') -> 'Double') -> 'State' ('Axis' b 'V2' n) ()
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_HeatMap_heatMapIndexedExample'.svg#diagram=heatMapIndexedExample'&height=350>>
--
-- > import Plots
-- > heatMapIndexedAxis' :: Axis B V2 Double
-- > heatMapIndexedAxis' = r2Axis &~ do
-- >   display colourBar
-- >   axisExtend .= noExtend
-- >   axisColourMap .= Plots.magma
-- >
-- >   let f (V2 x y) = fromIntegral x + fromIntegral y
-- >   heatMapIndexed' (V2 3 3) f
--
-- > heatMapIndexedExample' = renderAxis heatMapIndexedAxis'
--
heatMapIndexed'
  :: (VectorLike V2 Int i,
      MonadState (Axis V2) m)
  => i             -- ^ extent of array
  -> (i -> Double) -- ^ heat from index
  -> m ()          -- ^ add plot to 'Axis'
heatMapIndexed' i f = heatMapIndexed i f (return ())

-- heatMapRender :: Functor f => LensLike' f a (HeatMatrix -> ColourMap -> Diagram (V a))

-- surfaceRender :: HeatMatrix -> ColourMap -> Diagram V3
-- surfaceRender hm cm =

