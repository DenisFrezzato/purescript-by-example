module Recursion where

import Prelude

import Data.Array (null, filter)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

countEven :: Array Int -> Int
countEven = length <<< filter isEven 
  where
    isEven :: Int -> Boolean
    isEven n = n `mod` 2 == 0