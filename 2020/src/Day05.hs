module Day05 where

import qualified Control.Foldl        as L
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Parsing

pSeats :: Parser [Int]
pSeats = seat `sepEndBy` newline <* eof
 where
  pDigit = ("B" <|> "R") $> 1 <|> ("F" <|> "L") $> 0
  seat   = replicateM 10 pDigit <&> foldl' (\acc d -> acc * 2 + d) 0

findMySeat :: [Int] -> Maybe Int
findMySeat = L.fold (math <$> L.minimum <*> L.maximum <*> L.sum <*> L.length)
 where
  math mlo mhi total len = do
    lo <- mlo
    hi <- mhi
    pure $ (hi + lo) * (len + 1) `div` 2 - total

main :: Text -> IO ()
main input = do
  seats <- parse' pSeats input
  print $ maximumMay seats
  print $ findMySeat seats
