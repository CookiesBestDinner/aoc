{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Attoparsec.Text
import Data.Function (on)
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  input <- TIO.getContents
  let entries = case parseOnly (pLine `sepBy` endOfLine) input of
        Right entry -> entry
        Left  err   -> error err
      whoWhatUhmm = makeTree entries
  -- print entries
  -- putStrLn ""
  -- print whoWhatUhmm
  -- putStrLn ""
  let rootName = findParent whoWhatUhmm
      betterWeight = (Maybe.fromJust . fst) $ findBadWeight rootName $ makeBetterTree entries
  putStrLn $ "Part 1: " ++ rootName
  putStrLn $ "Part 2: " ++ show (betterWeight)

-- | given one of those up-side down trees, this picks an arbitrary node and
-- follows it until the root is found.
findParent :: Map String String -> String
findParent tree | null tree = undefined
                | otherwise = go mommy
 where
  (_, mommy) = head $ Map.assocs tree
  go parent = case Map.lookup parent tree of
    Nothing      -> parent
    Just grandpa -> go grandpa

-- | How is this not in some library, or more like where is it? Because surely
-- it does exist somewhere.
mostCommon :: Ord a => [a] -> a
mostCommon xs = winner
 where
  pairs       = [ (key, 1 :: Int) | key <- xs ]
  counts      = Map.fromListWith (+) pairs
  (winner, _) = List.maximumBy (compare `on` snd) (Map.assocs counts)

-- | What happens when I give up and insert code with no overall design.
findBadWeight :: String -> Map String Node -> (Maybe Int, Int)
findBadWeight root tree = if (not . null) answerOrEmpty
  then (head $ answerOrEmpty, 0)
  else case maybOffWeight of
    Nothing -> (Nothing, weight myself + sum weights)
    Just offendingWeight ->
      let diff     = commonWeight - offendingWeight
          whoDidIt = Maybe.fromJust
            $ List.findIndex ((== offendingWeight) . snd) childResults
          giveMeaNAME       = children !! whoDidIt
          datacard          = tree Map.! giveMeaNAME
          theirWeight       = weight datacard
          theirBetterWeight = theirWeight + diff
      in  (Just theirBetterWeight, 0)
 where
  myself        = tree Map.! root
  children      = getChildren myself
  childResults  = [ findBadWeight childname tree | childname <- children ]
  weights       = map snd childResults
  commonWeight  = mostCommon weights
  maybOffWeight = List.find (/= commonWeight) weights
  answerOrEmpty = map Just $ flip
    Maybe.mapMaybe
    childResults
    ( \x -> case x of
      (Just answer, _) -> Just answer
      (Nothing    , _) -> Nothing
    )

-- | My association list is a Map String Node
data Node = Node { weight :: Int
                 , getChildren :: [String]
                 }

-- | Less up-side down. Uses an association list instead of an actual tree
-- because otherwise I'd have to figure out the whole structure before
-- starting to build it...I think.
makeBetterTree :: [(String, Int, [String])] -> Map String Node
makeBetterTree []          = Map.empty
makeBetterTree (e:entries) = Map.insert name node tree
 where
  tree                  = makeBetterTree entries
  (name, val, children) = e
  node                  = Node val children

-- | Makes an up-side down tree..or something. following it leads to the root.
makeTree :: [(String, Int, [String])] -> Map String String
makeTree []          = Map.empty
makeTree (e:entries) = Map.union (Map.fromList pairs) tree
 where
  tree                = makeTree entries
  (base, _, children) = e
  pairs               = [ (child, base) | child <- children ]

pLine :: Parser (String, Int, [String])
pLine =
  (,,)
    <$> progName
    <*  " ("
    <*> decimal
    <*  ")"
    <*  option undefined " -> "
    <*> sepBy  progName  ", "
  where progName = many1 letter
