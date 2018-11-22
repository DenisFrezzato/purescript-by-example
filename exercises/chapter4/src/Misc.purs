module Misc where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, (..), length)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product, foldl)

sqr :: Array Int -> Array Int
sqr a = (\n -> n * n) <$> a

removeNegative :: Array Int -> Array Int
removeNegative = filter (\n -> n > 0) 

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors' n) == 1

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct a b = do
  i <- a
  j <- b
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\xs x -> xs && x) true

count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
    count' :: Int -> (a -> Boolean) -> Array a -> Int
    count' acc p [] = acc
    count' acc p xs = if p (unsafePartial head xs)
      then count' (acc + 1) p (unsafePartial tail xs)
      else count' acc p (unsafePartial tail xs)