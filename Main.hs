import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Puck" (640, 480) (5, 5))
               black
               frame

frame :: Float -> Picture
frame t = Translate (size * negate 4) (size * negate 4) $ tiles 8 8

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
