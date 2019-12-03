{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day02 where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Prelude                                  ( read )
import           Protolude

main :: IO ()
main = do
  input <- getContents
  let xs :: [Int] = input & T.unpack & (\s -> "[" ++ s ++ "]") & read
  let initialMem  = Map.fromList $ zip [0 ..] xs
  let mkComp a b = Comp 0 (initialMem & Map.insert 1 a & Map.insert 2 b)
  run (mkComp 12 2) & print
  print
    [ a * 100 + b
    | a <- [0 .. 99]
    , b <- [0 .. 99]
    , let result = mkComp a b & run
    , result == 19690720
    ]

data Comp =
  Comp
    { pc  :: Int
    , mem :: Map.Map Int Int
    }
  deriving (Show)

run :: Comp -> Int
run c = case step c of
  Right c' -> run c'
  Left  c' -> c' & mem & (Map.! 0)

step :: Comp -> Either Comp Comp
step c@(Comp p m) = case i of
  1  -> Right $ Comp (p + 4) (Map.insert o (a + b) m)
  2  -> Right $ Comp (p + 4) (Map.insert o (a * b) m)
  99 -> Left c
 where
  [i, a', b', o] = [ m Map.! n | n <- [p .. p + 3] ]
  a              = m Map.! a'
  b              = m Map.! b'
