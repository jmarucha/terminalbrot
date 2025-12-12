module Mandelbrot where

import Data.Complex

mandelbrotIter :: (RealFloat a) => Complex a -> Complex a -> Complex a
mandelbrotIter c z = c + z * z

mandelbrotEscapeTime :: (RealFloat a, Integral i) => i -> Complex a -> (Complex a, i)
mandelbrotEscapeTime n c =
  until
    (\(z, nn) -> magnitude z > 10 || nn == 0)
    (\(z', n') -> (mandelbrotIter c z', n' - 1))
    (0, n)

mandelbrotGraphData :: (RealFloat a, Integral i) => i -> Complex a -> i
mandelbrotGraphData i c =
  let (_, iter_left) = mandelbrotEscapeTime i c
   in iter_left
