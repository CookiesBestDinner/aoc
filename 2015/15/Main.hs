{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Arrow
import           Data.Function                            ( (&) )
import           Data.List.Extra                          ( maximumOn )

data Goop =
  Goop
    { name       :: String
    , capacity   :: Integer
    , durability :: Integer
    , flavor     :: Integer
    , texture    :: Integer
    , calories   :: Integer
    }
  deriving (Show)

score :: Goop -> Integer
score g = max 0 (capacity g) * max 0 (durability g) * max 0 (flavor g) * max
  0
  (texture g)

add :: Goop -> Goop -> Goop
add (Goop n1 c1 d1 f1 t1 ca1) (Goop n2 c2 d2 f2 t2 ca2) =
  Goop (n1 <> "|" <> n2) (c1 + c2) (d1 + d2) (f1 + f2) (t1 + t2) (ca1 + ca2)

mul :: Integer -> Goop -> Goop
mul n (Goop name capacity durability flavor texture calories) = Goop
  (show n <> "x(" <> name <> ")")
  (n * capacity)
  (n * durability)
  (n * flavor)
  (n * texture)
  (n * calories)

main = do
  ingredients <- (lines >>> map parseGoop) <$> getContents
  let candidates = bake 100 ingredients
      amazing    = maximumOn score candidates
  print "The winning recipie for part 1:"
  print amazing
  print (score amazing)
  -- part 2: Additional condition: must have exactly 500 calories
  let mealReplacement =
        candidates & filter (calories >>> (== 500)) & maximumOn score
  print "The winning recipie for part 2:"
  print mealReplacement
  print (score mealReplacement)

bake :: Integer -> [Goop] -> [Goop]
bake _    []          = []
bake room [i]         = [mul room i]
bake room ingredients = do
  let ingredient = head ingredients
  amount <- [0 .. room]
  future <- bake (room - amount) (tail ingredients)
  return $ add (mul amount ingredient) future

parseGoop :: String -> Goop
parseGoop ln = Goop { name, capacity, durability, flavor, texture, calories }
 where
  tokens     = words ln
  name       = init $ tokens !! 0
  capacity   = read $ init $ tokens !! 2
  durability = read $ init $ tokens !! 4
  flavor     = read $ init $ tokens !! 6
  texture    = read $ init $ tokens !! 8
  calories   = read $ tokens !! 10
