import Data.List
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- getContents
  let ls       = lines input
      parsed   = map words ls
      valid    = filter isValid parsed
      anaValid = filter isValid $ map (map sort) parsed
  print $ length valid
  print $ length anaValid


isValid :: [String] -> Bool
isValid wrds = length wrds == (length $ Set.toList $ Set.fromList wrds)
