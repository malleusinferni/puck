module State where

import Graphics.Gloss.Data.Color

type Palette = [Color]

type TileMap = [[Int]]

data Editor = Editor
  { cursor :: (Int, Int)
  , width :: Int
  , height :: Int
  , pixels :: TileMap
  , palette :: Palette
  }
