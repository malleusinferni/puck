import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Codec.BMP (packRGBA32ToBMP, writeBMP)
import qualified Data.ByteString as B (pack)

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List (unfoldr)

import State

main :: IO ()
main = playIO window black fps world
         (evalStateT frame) (execStateT . moveCursor) pass
  where fps = 30
        world = Editor (0, 0) 8 8 32 (allColors world) doubleRainbow

drawCursor :: World Picture
drawCursor = do
  size <- fromIntegral <$> gets zoomFactor
  place <- gets cursor
  let shapes = Pictures [outer, inner, connect]
      outer = Scale 1 (-1) $ thickArc 65 360 (size / 4) stroke
      inner = thickCircle (size / 8) stroke
      connect = Translate (size / 5) 0 . Scale (2/3) (5/4) $
        thickArc 180 360 (size / 8) stroke
      stroke = 5/3
  onPixel place . color white $ shapes

moveCursor :: Event -> World ()
moveCursor (EventKey (Char c) Down _ _) =
  case c of
    'h' -> go pred id
    'j' -> go id pred
    'k' -> go id succ
    'l' -> go succ id
    'y' -> go pred succ
    'u' -> go succ succ
    'b' -> go pred pred
    'n' -> go succ pred
    'a' -> paletteMod succ >>= modifyPixel
    'z' -> paletteMod pred >>= modifyPixel
    'w' -> saveImage
    _ -> return ()
  where go f g = do
          (x, y) <- (f *** g) <$> gets cursor
          w <- gets width
          h <- gets height
          when (0 <= x && x < w && 0 <= y && y < h) $
            modify (\world -> world { cursor = (x, y) })
moveCursor _ = return ()

saveImage :: World ()
saveImage = do
  w <- gets width
  h <- gets height
  -- Order of index generation is important!
  idxs <- mapM pixelAt [ (x, y) | y <- [ 0 .. h - 1 ], x <- [ 0 .. w - 1 ] ]
  let pixelToWord = map toWord . tupleToList . rgbaOfColor
      toWord = round . (* 255)
      tupleToList (a, b, c, d) = [a, b, c, d]
      raws = B.pack . concatMap pixelToWord $ idxs
  liftIO $ writeBMP "img.bmp" $ packRGBA32ToBMP w h raws

paletteMod :: (Int -> Int) -> World (Int -> Int)
paletteMod f = do
  len <- length <$> gets palette
  return (\i -> f i `mod` len)

modifyPixel :: (Int -> Int) -> World ()
modifyPixel f = do
  (x, y) <- gets cursor
  rows <- replace y (replace x f) <$> gets pixels
  modify (\world -> world { pixels = rows })

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

allColors :: Editor -> TileMap
allColors world = unfoldr go indices
  where go [] = Nothing
        go xs = Just $ splitAt w xs
        w = width world
        h = height world
        indices = map (`mod` palsize) [0 .. w * h - 1]
        palsize = length $ palette world

pass :: (Monad m) => a -> b -> m b
pass _ = return

window :: Display
window = InWindow "Puck" (640, 480) (5, 5)

frame :: World Picture
frame = do
  ts <- tiles
  c <- drawCursor
  return (Pictures (ts ++ [c]))

tiles :: World [Picture]
tiles = do
  w <- gets width
  h <- gets height
  size <- fromIntegral <$> gets zoomFactor
  let rect = rectangleSolid size size
  sequence [ onPixel (x, y) rect | x <- [0 .. pred w], y <- [0 .. pred h] ]

onPixel :: (Int, Int) -> Picture -> World Picture
onPixel (x, y) pic = do
  p <- pixelAt (x, y)
  size <- fromIntegral <$> gets zoomFactor
  bigx <- fromIntegral . (x -) . (`div` 2) <$> gets width
  bigy <- fromIntegral . (y -) . (`div` 2) <$> gets height
  return . Color p . Translate (size * bigx) (size * bigy) $ pic
