{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day02 where

import           Protolude
import           Prelude                                  ( read )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

main :: IO ()
main = do
  input <- getContents
  let xs :: [Int] = input & T.unpack & (\s -> "[" ++ s ++ "]") & read
  let initialMem  = Map.fromList $ zip [0 :: Int ..] xs
  let mkComp a b = Comp 0 patched2
       where
        patched1 = Map.insert 1 a initialMem
        patched2 = Map.insert 2 b patched1
  run (mkComp 12 2) & print
  print
    [ result
    | a <- [0 .. 99]
    , b <- [0 .. 99]
    , let result = mkComp a b & run
    , result == 19690720
    ]

data Comp = Comp {pc :: Int, getMem :: Map.Map Int Int} deriving Show

run :: Comp -> Int
run c = case step c of
  Right c' -> run c'
  Left  c' -> c' & getMem & (Map.! 0)

step :: Comp -> Either Comp Comp
step c@(Comp p m) = res
 where
  res = case i of
    1  -> Right $ Comp (p + 4) (Map.insert o (a + b) m)
    2  -> Right $ Comp (p + 4) (Map.insert o (a * b) m)
    99 -> Left c
  [i, a', b', o] = [ m Map.! n | n <- [p .. p + 3] ]
  a              = m Map.! a'
  b              = m Map.! b'
