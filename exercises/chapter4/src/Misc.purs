module Misc where

import Prelude

import Data.Array (filter, (..), length)
import Data.Foldable (product)
import Control.MonadZero (guard)

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