module Exercises where

import Prelude

newtype Complex = Complex {
  real :: Number,
  imaginary :: Number
}

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) =
    "Complex [real: " <> show real <> " imaginary: " <> show imaginary <> "]"

instance eqComplex :: Eq Complex where
  eq (Complex a) (Complex b) = a.real <= b.real && a.imaginary <= b.imaginary