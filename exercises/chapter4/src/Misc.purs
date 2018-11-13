module Misc where

import Prelude

import Data.Array (filter)

sqr :: Array Int -> Array Int
sqr a = (\n -> n * n) <$> a

removeNegative :: Array Int -> Array Int
removeNegative = filter (\n -> n > 0) 