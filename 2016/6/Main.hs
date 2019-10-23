{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Protolude
import           Control.Arrow
import           Data.List                                ( transpose
                                                          , sort
                                                          , group
                                                          )
import           Data.List.Extra                          ( maximumOn )
import qualified Data.Text                     as T

leastFrequent :: Ord a => [a] -> Maybe a
leastFrequent = sort >>> group >>> maximumOn (negate . length) >>> head

mostFrequent :: Ord a => [a] -> Maybe a
mostFrequent = sort >>> group >>> maximumOn length >>> head

main = do
  rows' <- T.lines <$> getContents
  let rows = map T.unpack rows'
  let cols = transpose rows
  print cols
  print $ catMaybes $ mostFrequent <$> cols
  print $ catMaybes $ leastFrequent <$> cols
