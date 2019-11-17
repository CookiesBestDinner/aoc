module Main where

import           Control.Arrow
import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.List        (foldl', minimumBy)
import           Data.Maybe       (maybeToList)
import qualified Data.Set         as Set
import           Text.Show.Pretty

data Spell
  = MagicMissile
  | Drain
  | Shield
  | Poison
  | Recharge
  deriving (Eq, Ord, Show)

data Part
  = Part1
  | Part2

data Effect =
  Effect
    { spell :: Spell
    , turns :: Int
    }
  deriving (Eq, Ord, Show)

data GameState =
  GameState
    { manaSpent :: Int
    , bossHp    :: Int
    , bossAtk   :: Int
    , playerHp  :: Int
    , playerMp  :: Int
    , effects   :: [Effect]
    , llog      :: [String]
    }
  deriving (Eq, Show)

newtype SearchQueue =
  SearchQueue (Set.Set GameState)

pureQueue :: GameState -> SearchQueue
pureQueue = SearchQueue . Set.fromList . (: [])

pop :: SearchQueue -> (GameState, SearchQueue)
pop (SearchQueue q) = Set.splitAt 1 q & first Set.findMin & second SearchQueue

push :: SearchQueue -> GameState -> SearchQueue
push (SearchQueue q) x = SearchQueue $ Set.insert x q

instance Ord GameState where
  compare =
    compare `on`
    (,,,,) <$> manaSpent <*> bossHp <*> playerHp <*> playerMp <*> effects

runSearch :: Part -> SearchQueue -> GameState
runSearch p q =
  case searchStep p q of
    Left g   -> g
    Right q' -> runSearch p q'

searchStep :: Part -> SearchQueue -> Either GameState SearchQueue
searchStep part q =
  case victories of
    [] -> Right $ foldl' push q' (resumes)
    ws -> ws & minimumBy (compare `on` manaSpent) & Left
  where
    victories = lefts outcomes & filter (playerHp >>> (> 0))
    resumes = rights outcomes
    (here', q') = pop q
    outcomes = do
      let here =
            case part of
              Part1 -> here'
              Part2 -> here' {playerHp = playerHp here' - 1}
      guard $ playerHp here > 0
      option <- [MagicMissile, Drain, Shield, Poison, Recharge]
      guard $ option `notElem` (effects here <&> spell)
      guard $ manaCost option <= playerMp here
      let there =
            case option of
              MagicMissile -> here {bossHp = bossHp here - 4}
              Drain ->
                here {bossHp = bossHp here - 2, playerHp = playerHp here + 2}
              eff -> here {effects = maybeToList (effect eff) ++ effects here}
      return $ do
        there
          { playerMp = playerMp here - manaCost option
          , manaSpent = manaSpent here + manaCost option
          , llog = show option : llog here
          } &
          runEffects >>=
          bossAttack >>=
          runEffects

bossAttack :: GameState -> Either GameState GameState
bossAttack game =
  let bossDmg =
        if effects game & filter (spell >>> (== Shield)) & (not . null)
          then max 1 (bossAtk game - 7)
          else bossAtk game
      game' = game {playerHp = playerHp game - bossDmg}
   in if playerHp game' <= 0
        then Left game'
        else Right game'

runEffects :: GameState -> Either GameState GameState
runEffects game =
  case applied of
    Left gs  -> Left gs
    Right gs -> Right $ gs {effects = effects'}
  where
    applied = foldM runEffect game (effects game)
    effects' = [Effect sp (n - 1) | Effect sp n <- effects game, n > 1]

runEffect :: GameState -> Effect -> Either GameState GameState
runEffect game eff
  | playerHp game' <= 0 = Left game
  | bossHp game' <= 0 = Left game'
  | otherwise = Right game'
  where
    game'
      | spell eff == Poison = game {bossHp = bossHp game - 3}
      | spell eff == Recharge = game {playerMp = playerMp game + 101}
      | otherwise = game

manaCost :: Num p => Spell -> p
manaCost MagicMissile = 53
manaCost Drain        = 73
manaCost Shield       = 113
manaCost Poison       = 173
manaCost Recharge     = 229

effect :: Spell -> Maybe Effect
effect Shield   = Just $ Effect Shield 6
effect Poison   = Just $ Effect Poison 6
effect Recharge = Just $ Effect Recharge 5
effect _        = Nothing

main :: IO ()
main = do
  mapM_ pPrint $ do
    part <- [Part1, Part2]
    return $
      runSearch part $
      pureQueue $
      GameState
        { manaSpent = 0
        , bossHp = 51
        , bossAtk = 9
        , playerHp = 50
        , playerMp = 500
        , effects = []
        , llog = []
        }
