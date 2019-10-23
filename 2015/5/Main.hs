module Main where
import           Control.Arrow
import qualified Data.Map.Strict               as Map
import           Control.Monad                            ( guard )
import           Data.List                                ( group
                                                          , isInfixOf
                                                          , tails
                                                          )
import           Data.Function

threeVowels :: String -> Bool
threeVowels = filter (`elem` "aoeui") >>> take 3 >>> length >>> (== 3)

twiceInARow :: String -> Bool
twiceInARow = group >>> filter (take 2 >>> length >>> (== 2)) >>> not . null

noBadSubs :: String -> Bool
noBadSubs s = isInfixOf <$> ["ab", "cd", "pq", "xy"] <*> [s] & or & not

main = do
  input <- getContents
  print $ length $ do
    s <- lines input
    guard $ [threeVowels, twiceInARow, noBadSubs] <*> [s] & and
  print $ length $ do
    s <- lines input
    guard $ [twicePairs, repeatWithOneBetween] <*> [s] & and

twicePairs :: String -> Bool
twicePairs s =
  Map.elems pairLocations
    & filter (maximum &&& minimum >>> uncurry (-) >>> (> 1))
    & null
    & not
 where
  pairLocations =
    s
      & tails
      & map (take 2)
      & filter (length >>> (== 2))
      & (`zip` [0 ..])
      & foldl addPairLoc Map.empty
  addPairLoc acc (pair, loc) =
    let oldLocs = Map.findWithDefault [] pair acc
    in  Map.insert pair (loc : oldLocs) acc

repeatWithOneBetween :: String -> Bool
repeatWithOneBetween =
  tails
    >>> map (take 3)
    >>> filter (length >>> (== 3))
    >>> filter (\[a, _, b] -> a == b)
    >>> not . null
