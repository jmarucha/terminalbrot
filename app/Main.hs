module Main where

import Camera
-- fuck off

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
import Prelude hiding (Left, Right)

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

{--
"\ESC[A" keyUp
"\ESC[B" keyDown
"\ESC[C" keyRight
"\ESC[D" keyLeft
--}

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
mainLoop hue camera = do
  let mainLoop' = mainLoop hue
  termSize <- getTerminalSize
  let (h, w) = case termSize of
        Just (h', w') -> (h', w')
        Nothing -> (25, 80)
  let viewport' = viewport w h 2 camera
      grid = complexGrid w h viewport' `using` rdeepseq
      iter_count = floor (zoom camera * (-50)) :: Integer
      charDraw = charMap hue iter_count . mandelbrotGraphData iter_count -- at list HLint is happy there is no brackets
      charsToDraw = concat $ parMap rpar (concatMap charDraw) grid
  setCursorPosition 0 0
  putStr charsToDraw

  setCursorPosition 0 0
  setSGR [SetRGBColor Foreground (hslColor hue 0.8 0.5)]
  putStrLn $ (" x" ++) $ showFFloat (Just 0) (exp ((-3) - zoom camera)) " "

  hFlush stdout

  key <- getKey
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

charMap :: Float -> Integer -> Integer -> [Char]
charMap _ _ 0 = " "
charMap hue i escape_time =
  let saturation = 1
      lightness = fromInteger (i - escape_time) / fromInteger i
      char = chr $ ord 'A' + mod (fromInteger escape_time) (ord 'Z' - ord 'A')
   in setSGRCode [SetRGBColor Foreground (hslColor hue saturation lightness)] ++ [char]

-- why is it so complicated?
hslColor :: Float -> Float -> Float -> Colour Float
hslColor h s l = uncurryRGB sRGB $ hsl h s l
