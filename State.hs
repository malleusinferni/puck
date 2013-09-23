module State where

import Graphics.Gloss.Data.Color

type Palette = [Color]

type TileMap = [[Int]]

data Editor = Editor
  { cursor :: (Int, Int)
  , width :: Int
  , height :: Int
  , zoomFactor :: Int
  , pixels :: TileMap
  , palette :: Palette
  }

pixelAt :: (Int, Int) -> Editor -> Color
pixelAt (x, y) world = palette world !! (pixels world !! y !! x)
