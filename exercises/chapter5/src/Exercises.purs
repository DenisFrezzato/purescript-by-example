module Exercises where

import Prelude

fact :: Int -> Int
fact 0 = 1
fact n
  | n < 0 = 1
  | otherwise = n * fact (n - 1)

binomialCoefficients :: Int -> Int -> Int
binomialCoefficients 0 _ = 1
binomialCoefficients 1 _ = 1
binomialCoefficients n k = fact n / (fact k * fact (n - k))