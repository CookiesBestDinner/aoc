{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Arrow
import           Data.Char       (isNumber)
import           Data.List       (iterate')
import           Data.List.Extra (maximumOn, minimumOn)
import qualified Data.Text       as T
import qualified Data.Text.Read  as TR
import           Protolude

main = do
  input <- getInput
  let victories = outfits & filter (\g -> playerWins input (g, 100))
  let losses = outfits & filter (\g -> not $ playerWins input (g, 100))
  print $ minimumOn cost victories
  print $ maximumOn cost losses

getInput = do
  input <- getContents
  let [hp, dmg, arm] =
        input & T.words & filter (T.all isNumber) & map TR.decimal &
        rights <&> fst
  print input
  return (Gear {cost = 0, damage = dmg, armor = arm}, hp)

data Fight =
  Fight
    { turn  :: Fighter
    , pGear :: Gear
    , pHP   :: Int
    , bGear :: Gear
    , bHP   :: Int
    }

data Fighter
  = Boss
  | Player

playerWins :: (Gear, Int) -> (Gear, Int) -> Bool
playerWins boss player =
  states & dropWhile (not . gameDone) & (\(x:_) -> x) & playerWon
  where
    f =
      Fight
        { turn = Player
        , pGear = fst player
        , pHP = snd player
        , bGear = fst boss
        , bHP = snd boss
        }
    states = iterate' fightTurn f
    playerWon = bHP >>> (< 1)
    playerLost = pHP >>> (< 1)
    gameDone = liftA2 (||) playerWon playerLost

fightTurn fight =
  case turn fight of
    Player -> fight {bHP = newBossHP, turn = Boss}
    Boss   -> fight {pHP = newPlayerHP, turn = Player}
  where
    newBossHP =
      bHP fight - max 1 ((damage . pGear $ fight) - (armor . bGear $ fight))
    newPlayerHP =
      pHP fight - max 1 ((damage . bGear $ fight) - (armor . pGear $ fight))

data Gear =
  Gear
    { cost   :: Int
    , damage :: Int
    , armor  :: Int
    }
  deriving (Eq, Ord, Show)

instance Semigroup Gear where
  (<>) a b =
    Gear
      { cost = sum (cost <$> [a, b])
      , damage = sum (damage <$> [a, b])
      , armor = sum (armor <$> [a, b])
      }

instance Monoid Gear where
  mempty = Gear 0 0 0

rings =
  [ Gear {cost = 25, damage = 1, armor = 0}
  , Gear {cost = 50, damage = 2, armor = 0}
  , Gear {cost = 100, damage = 3, armor = 0}
  , Gear {cost = 20, damage = 0, armor = 1}
  , Gear {cost = 40, damage = 0, armor = 2}
  , Gear {cost = 80, damage = 0, armor = 3}
  ]

armors =
  [ Gear {cost = 13, damage = 0, armor = 1}
  , Gear {cost = 31, damage = 0, armor = 2}
  , Gear {cost = 53, damage = 0, armor = 3}
  , Gear {cost = 75, damage = 0, armor = 4}
  , Gear {cost = 102, damage = 0, armor = 5}
  ]

weapons =
  [ Gear {cost = 8, damage = 4, armor = 0}
  , Gear {cost = 10, damage = 5, armor = 0}
  , Gear {cost = 25, damage = 6, armor = 0}
  , Gear {cost = 40, damage = 7, armor = 0}
  , Gear {cost = 74, damage = 8, armor = 0}
  ]

combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = do
  here <- [[], [x]]
  rest <- combinations (k - length here) xs
  return $ here ++ rest

options amounts source = do
  amount <- amounts
  selection <- combinations amount source
  return $ mconcat selection

outfits :: [Gear]
outfits = do
  a <- options [0 .. 2] rings
  b <- options [0 .. 1] armors
  c <- options [1 .. 1] weapons
  return $ a <> b <> c
