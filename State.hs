module State where

import Control.Monad.Trans.State
import Graphics.Gloss.Data.Color

type Palette = [Color]

type TileMap = [[Int]]

type World a = StateT Editor IO a

data Editor = Editor
  { cursor :: (Int, Int)
  , width :: Int
  , height :: Int
  , zoomFactor :: Int
  , pixels :: TileMap
  , palette :: Palette
  }

pixelAt :: (Int, Int) -> World Color
pixelAt (x, y) = do
  pal <- gets palette
  pix <- gets pixels
  return $ pal !! (pix !! y !! x)
