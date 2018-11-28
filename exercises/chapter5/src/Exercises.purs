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

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: cityA } } { address: { city: cityB } } = cityA == cityB

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a