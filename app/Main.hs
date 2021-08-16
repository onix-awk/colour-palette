{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Codec.Picture

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
extractPalette n _image = ColorPalette (replicate n black)
  where
    black = PixelRGB8 0 0 0

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
