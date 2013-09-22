import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import State

main :: IO ()
main = play window black fps world frame pass pass
  where fps = 30
        world = Editor (0, 0) 8 8

drawCursor :: Editor -> Picture
drawCursor world = inPlace $ color white $ thickCircle (size / 4) 2
  where inPlace = onPixel (cursor world) world

pass :: a -> b -> b
pass = flip const

window :: Display
window = InWindow "Puck" (640, 480) (5, 5)

frame :: Editor -> Picture
frame world = Pictures $ tiles world ++ [drawCursor world]

tiles :: Editor -> [Picture]
tiles world = [ go x y | x <- [0 .. pred w], y <- [0 .. pred h] ]
  where go x y =
          Color (makeColor (down w x) (up w x) (up h y) 1) $
          onPixel (x, y) world $
          (rectangleSolid size size)
        w = width world
        h = height world
        up k i = fromIntegral i / fromIntegral k
        down k i = fromIntegral (k - i) / fromIntegral k

onPixel :: (Int, Int) -> Editor -> Picture -> Picture
onPixel (x, y) world = Translate (size * bigx) (size * bigy)
  where bigx = fromIntegral $ x - width world `div` 2
        bigy = fromIntegral $ y - height world `div` 2

size :: Num a => a
size = 32
