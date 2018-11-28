module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Path (Path, ls, isDirectory, size, filename, root)
import Data.Array (concatMap, (:), filter, foldl, head)
import Data.Maybe (Maybe(..))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles'

largest :: Path -> Maybe Path
largest = foldl largest' Nothing <<< onlyFiles
  where
    largest' Nothing f = Just f
    largest' (Just acc) f = if size f > size acc
      then Just f
      else Just acc

smallest :: Path -> Maybe Path
smallest = foldl smallest' Nothing <<< onlyFiles
  where
    smallest' Nothing f = Just f
    smallest' (Just acc) f = if size f < size acc
      then Just f
      else Just acc

whereIs :: String -> Maybe Path
whereIs filePath = head $ do
  file <- allFiles' root
  child <- ls file
  guard $ filename child == filePath
  pure file
