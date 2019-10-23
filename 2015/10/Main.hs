module Main where

import           Control.Arrow
import           Data.List                                ( group
                                                          , iterate'
                                                          )
import           Data.Char                                ( isNumber )
import           Data.Function                            ( (&) )

main = do
  input <- getContents
  let ns = input & filter isNumber & map ((: []) >>> read)
  print $ length $ iterate' step ns !! 40
  print $ length $ iterate' step ns !! 50

step :: [Int] -> [Int]
step
  =   group
  >>> map (\xs -> [length xs, head xs])
  >>> concat
