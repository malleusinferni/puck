import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play window black fps () frame pass pass
  where fps = 30

pass :: a -> b -> b
pass = flip const

window :: Display
window = InWindow "Puck" (640, 480) (5, 5)

frame :: a -> Picture
frame _ = Translate (size * negate 4) (size * negate 4) $ tiles 8 8

tiles :: Int -> Int -> Picture
tiles w h = Pictures [ go x y | x <- [0 .. pred w], y <- [0 .. pred h] ]
  where go x y =
          Color (makeColor (down w x) (up w x) (up h y) 1) $
          Translate (big x) (big y) $
          (rectangleSolid size size)
        up k i = fromIntegral i / fromIntegral k
        down k i = fromIntegral (k - i) / fromIntegral k
        big = fromIntegral . (* size)

size :: Num a => a
size = 32
