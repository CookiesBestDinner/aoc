{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day02 where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Prelude                                  ( read
                                                          , error
                                                          )
import           Protolude

-- |
-- >>> readFile "input/day02" >>= main
-- 7594646
-- [3376]
main :: Text -> IO ()
main input = do
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
  _  -> error "bad instruction"
 where
  i = m Map.! p
  a = m Map.! (m Map.! (p + 1))
  b = m Map.! (m Map.! (p + 2))
  o = m Map.! (p + 3)
