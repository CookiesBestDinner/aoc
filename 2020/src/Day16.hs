module Day16 where

import           Data.Char
import           Data.Ix                    (inRange)
import           Data.List                  ((!!))
import qualified Data.Set as Set
import           Data.List.Extra            (chunksOf, groupOn)
import qualified Data.Text                  as T
import qualified Prelude                    as P
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

lnToRange s = [(a, b), (c, d)]
 where
  [a, b, c, d] :: [Int] = groupOn isDigit s & filter (all isDigit) <&> P.read

nearbyNumsP :: Parser [[Int]]
nearbyNumsP = do
  "nearby tickets:"
  space
  let row = decimal `sepEndBy` ","
  row `sepEndBy` eol

myticketP :: Parser [Int]
myticketP = do
  "your ticket:"
  space
  decimal `sepEndBy` "," <* space <* eof


possiblyValid :: [(Int, Int)] -> Int -> Bool
possiblyValid ranges n = not $ null $ do
  (lo, hi) <- ranges
  guard $ inRange (lo, hi) n
  pure ()

untangle :: [[Int]] -> [[Int]]
untangle = go mempty
  where
    go seen (xs: xss) = xs' : go newseen xss
      where
        xs' = filter (`Set.notMember`seen) xs
        inserts = Set.fromList xs'
        newseen = seen <> inserts
    go seen [] = []

main :: Text -> IO ()
main input = do
  let [fields, mine, others] = T.splitOn "\n\n" input
  let ranges = fields & T.splitOn "\n" <&> toS <&> lnToRange & mconcat
  othertickets <- parse' nearbyNumsP others
  print $ othertickets & mconcat & filter (not . possiblyValid ranges) & sum
  let hasInvalid :: [Int] -> Bool = not . all (possiblyValid ranges)
  let validOthers                 = filter (not . hasInvalid) othertickets & filter (not .null)
  let identifyPos [range1, range2] = do
        pos <- [0 .. 19]
        let values :: [Int] = validOthers <&> (!! pos)
        let isok val = inRange range1 val || inRange range2 val
        guard $ all isok values
        pure pos
  -- part2
  print "--"
  let order = chunksOf 2 ranges <&> identifyPos & sortOn length & untangle & concat
  let something = chunksOf 2 ranges <&> identifyPos & zip [0..] & sortOn (length . snd)
  let lsts = something <&> snd & untangle
  let is = fst <$> something
  let paired = zip is lsts & sortOn fst
  let betterorder = paired <&> snd & concat
  let want = take 6 betterorder
  mytick <- parse' myticketP mine
  let get i = mytick !! i
  print ("my ticket", mytick)
  print ("want", want)
  print $ want <&> get
  print $ want <&> get & product
  print $ want <&> get <&> toInteger & product
