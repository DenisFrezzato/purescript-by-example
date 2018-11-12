module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Path (root)
import Data.Foldable (for_)
import FileOperations (allFiles)

main :: Effect Unit
main = for_ (allFiles root) logShow
