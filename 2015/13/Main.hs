module Main where

import qualified Data.Map.Strict               as Map
import           Data.List                                ( nub
                                                          , permutations
                                                          )
import           Data.List.Extra                          ( nub
                                                          , permutations
                                                          , maximumOn
                                                          )

parseRelation :: String -> ((String, String), Int)
parseRelation ln = ((name1, name2), delta)
 where
  tokens     = words ln
  name1      = tokens !! 0
  name2      = init $ tokens !! 10
  gainorlose = tokens !! 2
  dist       = read $ tokens !! 3
  delta      = case gainorlose of
    "gain" -> dist
    "lose" -> negate dist
    _ -> error $ "There was a problem parsing this relation line: " <> show ln

collectiveHappiness :: Map.Map (String, String) Int -> [String] -> Int
collectiveHappiness happyNotes (a : b : rest) = rel1 + rel2 + continue
 where
  rel1     = happyNotes Map.! (a, b)
  rel2     = happyNotes Map.! (b, a)
  continue = collectiveHappiness happyNotes (b : rest)
collectiveHappiness _ _ = 0

main = do
  relations <- map parseRelation . lines <$> getContents
  let people = nub $ do
        ((a, b), _) <- relations
        [a, b]
      candidates = makeCircularish <$> permutations people
      makeCircularish xs = xs <> [head xs]
      happyNotes = Map.fromList relations
  print relations
  let bestSeating = maximumOn (collectiveHappiness happyNotes) candidates
  print bestSeating
  print $ collectiveHappiness happyNotes bestSeating
  -- Part 2: add myself to the list as a neutral person
  let addendum = do
        p <- people
        [(("Me", p), 0), ((p, "Me"), 0)]
      relations'   = relations <> addendum
      people'      = people <> ["Me"]
      candidates'  = makeCircularish <$> permutations people'
      happyNotes'  = Map.fromList relations'
      bestSeating' = maximumOn (collectiveHappiness happyNotes') candidates'
  print bestSeating'
  print $ collectiveHappiness happyNotes' bestSeating'
