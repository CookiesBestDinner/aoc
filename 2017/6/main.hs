import Data.Attoparsec.Text (Parser, decimal, parseOnly, sepBy, space)
import Data.Either (fromRight)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Set (Set, empty, insert, member)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  input <- TIO.getContents
  let blocks = fromRight undefined $ parseOnly pInput input
      allStates  = run empty blocks
      part1 = length allStates - 1
  print part1
  let firstRepeated = last allStates
      whenFirstSeen = findIndex (== firstRepeated) allStates
      cycleLength = length allStates - (fromJust whenFirstSeen) - 1
      part2 = cycleLength
  print part2

run :: Set [Int] -> [Int] -> [[Int]]
run seen blocks | blocks `member` seen = [blocks]
                | otherwise = blocks : run (insert blocks seen) newBlocks
 where
  maxBlock     = maximum blocks
  maxWhere     = fromJust $ findIndex (== maxBlock) blocks
  len          = length blocks
  addToEach    = maxBlock `div` len
  extras       = maxBlock `rem` len
  zeroMax      = (take maxWhere blocks) ++ 0 : (drop (maxWhere + 1) blocks)
  incrementAll = map (+ addToEach) zeroMax
  extrasEnd    = seg1 ++ (map (+ 1) seg2) ++ seg3
   where
    seg1 = take (maxWhere + 1) incrementAll
    seg2 = take extras $ drop (maxWhere + 1) incrementAll
    seg3 = drop (maxWhere + 1 + extras) incrementAll
  extrasStart =
    (map (+ 1) $ take remaining extrasEnd) ++ drop remaining extrasEnd
   where
    availableEnd = len - maxWhere - 1
    remaining    = max 0 $ extras - availableEnd
  newBlocks = if extras > 0 then extrasStart else incrementAll

pInput :: Parser [Int]
pInput = decimal `sepBy` space
