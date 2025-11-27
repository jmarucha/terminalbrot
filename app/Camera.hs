module Camera where

import Data.Complex (Complex (..))
import Prelude hiding (Left, Right) -- fuck off

data Direction
  = Left
  | Right
  | Up
  | Down

data Camera = Camera
  { pos_x :: Double,
    pos_y :: Double,
    zoom :: Double
  }
  deriving (Show)

move :: Camera -> Direction -> Camera
move camera Left = Camera (pos_x camera - velocity camera) (pos_y camera) (zoom camera)
move camera Right = Camera (pos_x camera + velocity camera) (pos_y camera) (zoom camera)
move camera Down = Camera (pos_x camera) (pos_y camera - velocity camera) (zoom camera)
move camera Up = Camera (pos_x camera) (pos_y camera + velocity camera) (zoom camera)

zoomIn :: Camera -> Camera
zoomIn camera = Camera (pos_x camera) (pos_y camera) (zoom camera - 0.2)

zoomOut :: Camera -> Camera
zoomOut camera = Camera (pos_x camera) (pos_y camera) (zoom camera + 0.2)

velocity :: Camera -> Double
velocity = (10 *) . exp . zoom

viewport :: (Integral size) => size -> size -> Double -> Camera -> ((Double, Double), (Double, Double))
viewport w h aspect_ratio camera =
  let x_center = pos_x camera
      y_center = pos_y camera
      char_size_x = exp $ zoom camera
      char_size_y = char_size_x * aspect_ratio
   in ( ( x_center - char_size_x * fromIntegral (div w 2),
          x_center + char_size_x * fromIntegral (div w 2)
        ),
        ( y_center - char_size_y * fromIntegral (div h 2),
          y_center + char_size_y * fromIntegral (div h 2)
        )
      )

complexGrid :: (Integral size) => size -> size -> ((Double, Double), (Double, Double)) -> [[Complex Double]]
complexGrid w h (range_x, range_y) =
  [[x :+ y | x <- oneDimGrid w range_x] | y <- reverse $ oneDimGrid h range_y]

oneDimGrid :: (Integral size) => size -> (Double, Double) -> [Double]
oneDimGrid len (a, b) =
  let step = (b - a) / fromIntegral (len - 1)
      go len' a' b' acc
        | len' == 0 = acc
        | otherwise = go (len' - 1) a' (b' - step) (b' : acc)
   in go len a b []
