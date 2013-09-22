module State where

data Editor = Editor
  { cursor :: (Int, Int)
  , width :: Int
  , height :: Int
  }
