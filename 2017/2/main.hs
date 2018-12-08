import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  input <- TIO.getContents
  let rows  = fromRight $ parseOnly pRows input
      diffs = map (\row -> maximum row - minimum row) rows
  print $ sum diffs
  let part2 =
        sum
        [a `div` b
        | row <- rows
        , a <- row
        , b <- row
        , a /= b
        , a `rem` b == 0
        ]
  print part2

fromRight :: Either a b -> b
fromRight (Right v) = v

pRows :: Parser [[Integer]]
pRows = decimal `sepBy1` satisfy isHorizontalSpace `sepBy` endOfLine
