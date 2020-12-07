module Day06 where

import           Data.Char
import           Data.List (foldl1')
import qualified Data.Set  as Set
import qualified Data.Text as Text
import           Protolude

groupCount :: [[Char]] -> Int
groupCount = Set.size . Set.fromList . mconcat

groupCount2 :: [[Char]] -> Int
groupCount2 grp = Set.size $ foldl1' Set.intersection (Set.fromList <$> grp)

main :: Text -> IO ()
main input = do
  let groups = Text.splitOn "\n\n" input <&> lines <&> map toS
  print $ sum $ groupCount <$> groups
  print $ sum $ groupCount2 <$> groups
