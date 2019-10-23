{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Function
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import Debug.Trace

main = do
  input <- T.lines <$> TIO.getContents
  let medicine = last input
  let transformPairs :: [Map.Map T.Text [T.Text]]
      transformPairs = do
        transform <- reverse input & drop 2
        let [from, _arrow, to] = T.words transform
        [Map.fromList [(from, [to])]]
      transformMap       = Map.unionsWith (<>) transformPairs
      possibleTransforms = synthesizeStep transformMap medicine
  print $ Set.size possibleTransforms
  -- print $ searchFor 0 medicine transformMap Set.empty (Set.fromList ["e"])
  print $ searchFor2 0 medicine transformMap (Set.fromList ["e"])

searchFor n medicine transformations seen input
  | medicine `Set.member` input
  = 0
  | otherwise
  = let newOptions = Set.unions $ map (synthesizeStep transformations) (Set.elems input)
        withoutSeen = Set.difference newOptions seen
        newSeen = Set.union seen withoutSeen
    in  traceShow n $ searchFor (n+1) medicine transformations newSeen withoutSeen

searchFor2 n medicine transformations input
  | medicine `Set.member` input
  = 0
  | otherwise
  = let newOptions = Set.unions $ map (synthesizeStep transformations) (Set.elems input)
    in  traceShow n searchFor2 (n+1) medicine transformations newOptions




synthesizeStep transformations input = Set.fromList $ do
  -- pick a key
  (from, tos)    <- Map.assocs transformations
  -- and an outcome
  to             <- tos
  -- as well as a location in the medicine where that appears
  (front, back') <- T.breakOnAll from input
  let back = T.drop (T.length from) back'
  [T.concat [front, to, back]]
