{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Codec.Picture
import Data.Ord
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V

paletteSize :: Int
paletteSize = 5

main :: IO ()
main = do
  e <- readJpeg "input.jpeg"
  case e of
    Left str -> putStrLn str
    Right dynamicImage -> do
      let imageRGB8 = convertRGB8 dynamicImage
          palette = extractPalette paletteSize imageRGB8
          imageWithPalette = ImageRGB8 (attachPalette palette imageRGB8)
      saveJpgImage 100 "output.jpeg" imageWithPalette

-- | A color palette.
newtype ColorPalette = ColorPalette [PixelRGB8]
  deriving (Eq, Show)

-- | Extract the given number of primary colors from an image.
extractPalette ::
  -- | Number of colors to extract
  Int ->
  -- | The input image
  Image PixelRGB8 ->
  -- | The extracted palette
  ColorPalette
extractPalette n image = kClusterPalette n image topColors
  where
    colorBins = distributePixels n image
    topColors = extractTopColors n colorBins

-- | Distribute each pixel of a given image into a given number of bins per
-- dimension.
distributePixels ::
  -- | The bins
  Int ->
  -- | Input image
  Image PixelRGB8 ->
  -- | Number of bins per dimension
  Vector (Int, PixelRGB8)
distributePixels = undefined -- TODO

-- | Extract top colors based on their frequency.
extractTopColors ::
  -- | The number of colors to be extracted
  Int ->
  -- | The bins
  Vector (Int, PixelRGB8) ->
  -- | Top colors
  Vector PixelRGB8
extractTopColors n bins = V.take n (V.map selectSecond (sortVector f bins))
  where
    selectSecond (_, pixel) = pixel
    selectFirst (a, _) = a
    f = comparing (Down . selectFirst)

-- | Apply the k-means clustering algorithm on the given image.
kClusterPalette ::
  -- | The number of colors to extract (k)
  Int ->
  -- | Input image
  Image PixelRGB8 ->
  -- | The top colors to use for initialization of the k-means algorithm
  Vector PixelRGB8 ->
  -- | The resulting palette
  ColorPalette
kClusterPalette = undefined -- TODO

-- | Attach the given color palette to the given image.
attachPalette ::
  -- | Color palette
  ColorPalette ->
  -- | Original image
  Image PixelRGB8 ->
  -- | Image with the palette attached
  Image PixelRGB8
attachPalette palette originalImage@Image {..} =
  generateImage genFun targetWidth targetHeight
  where
    genFun :: Int -> Int -> PixelRGB8
    genFun x y =
      if x < imageWidth && y < imageHeight
        then pixelAt originalImage x y
        else getPaletteColor palette (x `div` cellSize)
    targetWidth = imageWidth
    targetHeight = imageHeight + cellSize
    cellSize = targetWidth `ceilDiv` cpSize
    cpSize = getPaletteSize palette

----------------------------------------------------------------------------
-- Helpers

getPaletteSize :: ColorPalette -> Int
getPaletteSize (ColorPalette l) = length l

getPaletteColor :: ColorPalette -> Int -> PixelRGB8
getPaletteColor (ColorPalette l) index = l !! index

ceilDiv :: Int -> Int -> Int
ceilDiv x y = (x + y - 1) `div` y

sortVector :: (a -> a -> Ordering) -> Vector a -> Vector a
sortVector f v = V.create $ do
  mv <- V.thaw v
  V.sortBy f mv
  return mv
