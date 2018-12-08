{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Function (on)
import Data.List
import Data.List.Extra
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Event = WakesUp     {when :: Integer}
           | FallsAsleep {when :: Integer}
           | NewShift    {who :: Integer}
           deriving (Show, Eq)

main :: IO ()
main = do
  -- sorting lines to obatain chronological order.
  events <- map (fromRight . parseOnly pEvent) . sort . T.lines <$> TIO.getContents
  let shifts = filter (not . null) $ splitIntoShifts events
      massagedSessions = map minutesSlept shifts
      joinedSessions = filter (not . null . snd)
                     $ map concatShifts
                     $ groupBy ((==) `on` fst)
                     $ sortBy (compare `on` fst) massagedSessions
      sleepiestGuard = maximumBy (compare `on` (length . snd)) joinedSessions
      sleepiestGuardId = fst sleepiestGuard
      mostCommonMinute = mostCommon $ snd sleepiestGuard
      part1 = sleepiestGuardId * mostCommonMinute
  print part1
  -- part 2
  let sleepiestMins = zip (map (mostCommonAndHowMany . snd) joinedSessions)
                          joinedSessions
      sortedByConsistency = sortOn (snd . fst) sleepiestMins
      mostConsistent = last sortedByConsistency
      ((guardId, _), (whichMinute, _)) = mostConsistent
      part2 = guardId * whichMinute
  print part2

-- for part2.. if only I had used some library frequency counter
mostCommonAndHowMany :: [Integer] -> (Integer, Integer)
mostCommonAndHowMany xs = (which, amount)
  where mostCommons = last $ sortOn length $ group $ sort xs
        amount = fromIntegral $ length mostCommons
        which = head mostCommons

mostCommon :: [Integer] -> Integer
mostCommon xs = head $ last $ sortOn length $ group $ sort xs

concatShifts :: [(Integer, [Integer])] -> (Integer, [Integer])
concatShifts shifts = (guardId, concatMap snd shifts)
  where guardId = fst $ head shifts

splitIntoShifts :: [Event] -> [[Event]]
splitIntoShifts = go [] []
  where go acc accSess [] = reverse (reverse accSess:acc)
        go acc accSess (nexthead@(NewShift _):rest) = go (reverse accSess:acc) [nexthead] rest
        go acc accSess (event:rest) = go acc (event:accSess) rest

minutesSlept :: [Event] -> (Integer, [Integer])
minutesSlept shift = (who $ head shift, minutes)
  where naps = chunksOf 2 $ tail shift
        minutes = concatMap howLong naps
        howLong [begin, end] = [when begin .. when end - 1]

fromRight :: Either String b -> b
fromRight (Right value) = value
fromRight (Left err) = error err

pEvent :: Parser Event
pEvent = do
  time <- (+) . (*60)
    <$  takeWhile1 (/= ' ')  -- ignore [year-month-day
    <*  char ' '
    <*> decimal
    <*  char ':'
    <*> decimal
    <*  string "] "
  eventType <- choice
    [ WakesUp <$ string "wakes up"
    , FallsAsleep <$ string "falls asleep"
    , const . NewShift
        <$  string "Guard #"
        <*> decimal
        <*  takeWhile1 (not . isEndOfLine)
    ]
  return $ eventType time
