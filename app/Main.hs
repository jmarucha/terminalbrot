module Main where

import Camera
import Control.Parallel.Strategies (parMap, rdeepseq, rpar, using)
import Data.Char (chr, ord)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.SRGB (Colour, sRGB)
import Mandelbrot (mandelbrotGraphData)
import Numeric (showFFloat)
import System.Console.ANSI
import System.IO
import System.Random (getStdGen, randomR)
import Prelude hiding (Left, Right) -- fuck off

-- stolen from Chris Stryczynski (https://stackoverflow.com/a/38553473)
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char : chars)

initialZoom :: Double
initialZoom = -3

main :: IO ()
-- Simple menu controller
main = do
  initScreen
  gen <- getStdGen

  let (hue, _) = randomR (0, 360) gen
  let initial_camera = Camera 0.0 0.0 initialZoom

  mainLoop hue initial_camera
  cleanupScreen

initScreen :: IO ()
initScreen = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  useAlternateScreenBuffer
  hideCursor

cleanupScreen :: IO ()
cleanupScreen = do
  useNormalScreenBuffer
  showCursor
  setSGR [Reset]

-- definitely not ordinary imperative loop
mainLoop :: Float -> Camera -> IO ()
mainLoop render_hue camera = do
  termSize <- getTerminalSize
  let (h, w) = case termSize of
        Just (h', w') -> (h', w')
        Nothing -> (24, 80) -- VT-100
  let viewport' = viewport w h 2 camera
      grid = complexGrid w h viewport' `using` rdeepseq
      iter_count = floor (zoom camera * (-50)) :: Int
      charDraw = charMap render_hue iter_count . mandelbrotGraphData iter_count
      charsToDraw = concat $ parMap rpar (concatMap charDraw) grid
  setCursorPosition 0 0
  putStr charsToDraw

  let zoomLevelColor = hslColor render_hue 0.8 0.5
      zoomLevel = exp ((-3) - zoom camera)
      precision
        | zoomLevel < 10 = 2
        | zoomLevel < 1000 = 1
        | otherwise = 0
  setCursorPosition 0 0
  setSGR [SetRGBColor Foreground zoomLevelColor]
  putStrLn $ (" x" ++) $ showFFloat (Just precision) zoomLevel " "

  hFlush stdout

  key <- getKey
  let mainLoop' = mainLoop render_hue
  {--
  "\ESC[A" keyUp
  "\ESC[B" keyDown
  "\ESC[C" keyRight
  "\ESC[D" keyLeft
  --}
  case key of
    "w" -> mainLoop' $ move camera Up
    "\ESC[A" -> mainLoop' $ move camera Up
    "s" -> mainLoop' $ move camera Down
    "\ESC[B" -> mainLoop' $ move camera Down
    "d" -> mainLoop' $ move camera Right
    "\ESC[C" -> mainLoop' $ move camera Right
    "a" -> mainLoop' $ move camera Left
    "\ESC[D" -> mainLoop' $ move camera Left
    "q" -> mainLoop' $ zoomIn camera
    "e" -> mainLoop' $ zoomOut camera
    "\ESC" -> return ()
    _ -> mainLoop' camera

charMap :: (Integral a, Integral a) => Float -> a -> a -> [Char]
charMap _ _ 0 = " "
charMap hue i escape_time =
  let saturation = 1
      lightness = fromIntegral (i - escape_time) / fromIntegral i
      char = chr $ ord 'A' + mod (fromIntegral escape_time) (ord 'Z' - ord 'A')
   in setSGRCode [SetRGBColor Foreground (hslColor hue saturation lightness)] ++ [char]

-- why is it so complicated?
hslColor :: Float -> Float -> Float -> Colour Float
hslColor h s l = uncurryRGB sRGB $ hsl h s l
