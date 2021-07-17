module Main (main) where

import Codec.Picture

main :: IO ()
main = do
  e <- readJpeg "input.jpeg"
  case e of
    Left str -> putStrLn str
    Right dynamicImage -> do
      saveJpgImage 100 "output.jpeg" dynamicImage
      putStrLn "I'm all done!"
    -- Right _ ->
    --   putStrLn "Unexpected color scheme of the image"
