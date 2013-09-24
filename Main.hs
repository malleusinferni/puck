import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Arrow ((***))
import Data.List (unfoldr)

import State

main :: IO ()
main = playIO window black fps world frame moveCursor pass
  where fps = 30
        world = Editor (0, 0) 8 8 32 allColors doubleRainbow

drawCursor :: Editor -> Picture
drawCursor world = inPlace $ color white $ shapes
  where inPlace = onPixel (cursor world) world
        shapes = Pictures [outer, inner, connect]
        outer = Scale 1 (-1) $ thickArc 65 360 (size / 4) stroke
        inner = thickCircle (size / 8) stroke
        connect = Translate (size / 5) 0 . Scale (2/3) (5/4) $
          thickArc 180 360 (size / 8) stroke
        stroke = (5/3)
        size = fromIntegral $ zoomFactor world

moveCursor :: Event -> Editor -> IO Editor
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
    _ -> return world
  where go f g = do
          let dest = f *** g $ cursor world
          return $ if bounded dest
                      then world { cursor = dest }
                      else world
        bounded (x, y) = 0 <= x && x < width world &&
          0 <= y && y < height world
moveCursor _ world = return world

modifyPixel :: (Int -> Int) -> Editor -> IO Editor
modifyPixel f world = return world { pixels = rows }
  where rows = replace y (replace x f) $ pixels world
        (x, y) = cursor world

replace :: Int -> (a -> a) -> [a] -> [a]
replace n f (x:xs)
  | n < 1 = f x : xs
  | otherwise = x : replace (n - 1) f xs
replace _ _ [] = []

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

pass :: (Monad m) => a -> b -> m b
pass _ = return

window :: Display
window = InWindow "Puck" (640, 480) (5, 5)

frame :: Editor -> IO Picture
frame world = return . Pictures $ tiles world ++ [drawCursor world]

tiles :: Editor -> [Picture]
tiles world = [ go x y | x <- [0 .. pred w], y <- [0 .. pred h] ]
  where go x y = onPixel (x, y) world $ rectangleSolid size size
        w = width world
        h = height world
        size = fromIntegral $ zoomFactor world

onPixel :: (Int, Int) -> Editor -> Picture -> Picture
onPixel (x, y) world = Color (pixelAt (x, y) world) .
    Translate (size * bigx) (size * bigy)
  where bigx = fromIntegral $ x - width world `div` 2
        bigy = fromIntegral $ y - height world `div` 2
        size = fromIntegral $ zoomFactor world
