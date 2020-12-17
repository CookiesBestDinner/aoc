module Day16 where

import           Control.Arrow
import           Data.Ix                    (inRange)
import           Data.List                  ((!!))
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Protolude                  hiding (many, try)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

data FieldSpec
  = FieldSpec
      { name :: Text
      , r1   :: (Int, Int)
      , r2   :: (Int, Int)
      }
  deriving (Show)

data Input
  = Input
      { fieldSpecs   :: [FieldSpec]
      , yourTicket   :: [Int]
      , otherTickets :: [[Int]]
      }

rangeP :: Parser FieldSpec
rangeP = try $ do
  name <- takeWhileP Nothing (`notElem` [':', '\n']) <* ": "
  r1   <- (,) <$> decimal <* "-" <*> decimal <* " or "
  r2   <- (,) <$> decimal <* "-" <*> decimal <* eol
  pure $ FieldSpec { .. }

inputP :: Parser Input
inputP = do
  let ticketP = decimal `sepBy` "," <* eol
  fieldSpecs <- many rangeP <* eol
  "your ticket:" >> eol
  yourTicket <- ticketP <* eol
  "nearby tickets:" >> eol
  otherTickets <- many ticketP <* eof
  pure $ Input { .. }

invalid :: [(Int, Int)] -> Int -> Bool
invalid ranges n = not (any matches ranges)
  where matches range = inRange range n

untangle :: [[Int]] -> [[Int]]
untangle css =
  go mempty (snd <$> byLenWPos) & zip (fst <$> byLenWPos) & sortOn fst <&> snd
 where
  byLenWPos = zip [0 :: Int ..] css & sortOn (length . snd)
  go seen (xs : xss) = xs' : go newseen xss
   where
    xs'     = filter (`Set.notMember` seen) xs
    newseen = seen <> Set.fromList xs'
  go _ [] = []

part2 :: Input -> Int
part2 indata =
  let ranges      = fieldSpecs indata >>= (\field -> [r1 field, r2 field])
      validTicket = not . any (invalid ranges)
      validOthers = filter validTicket (otherTickets indata)
      getPositions fields = untangle (getCandidates <$> fields)
      getCandidates field = do
        let width = validOthers <&> length & maximumDef 0
        pos <- [0 .. width - 1]
        let values = validOthers <&> (!! pos)
            isok val = inRange (r1 field) val || inRange (r2 field) val
        guard $ all isok values
        pure pos
  in  fieldSpecs indata
        &   getPositions
        &   zip [0 :: Int ..]
        &   sortOn snd
        &   zip (yourTicket indata)
        &   sortOn (fst . snd)
        <&> fst
        &   zip (fieldSpecs indata)
        &   filter (fst >>> name >>> T.isPrefixOf "departure")
        <&> snd
        &   product

main :: Text -> IO ()
main input = do
  indata <- parse' inputP input
  let ranges = fieldSpecs indata >>= (\field -> [r1 field, r2 field])
  print $ yourTicket indata
  print $ otherTickets indata & mconcat & filter (invalid ranges) & sum
  print $ part2 indata
