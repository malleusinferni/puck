import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Control.Arrow ((***))
import Data.List (unfoldr)

import State

main :: IO ()
main = play window black fps world frame moveCursor pass
  where fps = 30
        world = Editor (0, 0) 8 8 allColors doubleRainbow

drawCursor :: Editor -> Picture
drawCursor world = inPlace $ color white $ thickCircle (size / 4) 2
  where inPlace = onPixel (cursor world) world

moveCursor :: Event -> Editor -> Editor
moveCursor (EventKey (Char c) Down _ _) world =
  case c of
    'h' -> go pred id
    'j' -> go id pred
    'k' -> go id succ
    'l' -> go succ id
    'y' -> go pred succ
    'u' -> go succ succ
    'b' -> go pred pred
    'n' -> go succ pred
    'a' -> modifyPixel (\i -> succ i `mod` length (palette world)) world
    'z' -> modifyPixel (\i -> pred i `mod` length (palette world)) world
    _ -> world
  where go f g = let dest = f *** g $ cursor world in
          if bounded dest
             then world { cursor = dest }
             else world
        bounded (x, y) = 0 <= x && x < width world &&
          0 <= y && y < height world
moveCursor _ world = world

modifyPixel :: (Int -> Int) -> Editor -> Editor
modifyPixel f world = world { pixels = rows }
  where rows = replace y (replace x f) $ pixels world
        (x, y) = cursor world
        replace 0 f (x:xs) = f x : xs
        replace n f (x:xs) = x : replace (n - 1) f xs

doubleRainbow :: Palette
doubleRainbow = quadInterp 8 magenta cyan red green

quadInterp :: Int -> Color -> Color -> Color -> Color -> Palette
quadInterp n tl tr bl br = concat $ zipWith mix (mix bl tl) (mix br tr)
  where high = fromIntegral $ n - 1
        ratios = map (/ high) [0 .. high]
        mix l r = do
          v <- ratios
          let u = 1 - v
          return $ mixColors u v l r

allColors :: TileMap
allColors = unfoldr go [0 .. 8 * 8 - 1]
  where go [] = Nothing
        go xs = Just $ splitAt 8 xs

pass :: a -> b -> b
pass = flip const

window :: Display
window = InWindow "Puck" (640, 480) (5, 5)

frame :: Editor -> Picture
frame world = Pictures $ tiles world ++ [drawCursor world]

tiles :: Editor -> [Picture]
tiles world = [ go x y | x <- [0 .. pred w], y <- [0 .. pred h] ]
  where go x y = onPixel (x, y) world $ rectangleSolid size size
        w = width world
        h = height world

onPixel :: (Int, Int) -> Editor -> Picture -> Picture
onPixel (x, y) world = Color (pixelAt (x, y) world) .
    Translate (size * bigx) (size * bigy)
  where bigx = fromIntegral $ x - width world `div` 2
        bigy = fromIntegral $ y - height world `div` 2

size :: Num a => a
size = 32
