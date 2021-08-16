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
extractPalette = undefined -- TODO

-- | Attach the given color palette to the given image.
attachPalette ::
  -- | Color palette
  ColorPalette ->
  -- | Original image
  Image PixelRGB8 ->
  -- | Image with the palette attached
  Image PixelRGB8
attachPalette = undefined -- TODO
