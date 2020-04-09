{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Arrow
import           Control.Lens
import           Data.Char
import           Data.List.Extra                ( maximumOn )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.STRef
import           Protolude               hiding ( try )
import           System.Timeout
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Text.Pretty.Simple

type DamageType = Text
data Group = Group
  { _gunits      :: Int
  , _ghp         :: Int
  , _ginitiative :: Int
  , _gdmg        :: Int
  , _gdmgtype    :: DamageType
  , _gweak       :: [DamageType]
  , _gimmune     :: [DamageType]
  , _gteam       :: Text
  , _gid         :: Int
  } deriving (Eq, Show)
makeLenses ''Group

type Parser = Parsec Void Text

pDmgTypeList :: Parser [DamageType]
pDmgTypeList =
  let sep    = ", "
      damage = takeWhile1P Nothing isAlpha
  in  damage `sepBy1` sep

pDefences :: Parser ([DamageType], [DamageType])
pDefences = do
  let immuneSection = "immune to " >> Right <$> pDmgTypeList
      weakSection   = "weak to " >> Left <$> pDmgTypeList
      section       = immuneSection <|> weakSection
      sep           = "; "
  "("
  defences <- section `sepBy` sep
  ") "
  let weaknesses = mconcat $ lefts defences
      immunities = mconcat $ rights defences
  pure (weaknesses, immunities)

pGroup :: Text -> Parser Group
pGroup _gteam = do
  _gunits <- Lex.decimal
  " units each with "
  _ghp <- Lex.decimal
  " hit points "
  (_gweak, _gimmune) <- try pDefences <|> pure ([], [])
  "with an attack that does "
  _gdmg <- Lex.decimal
  " "
  _gdmgtype <- takeWhile1P Nothing isAlpha
  " damage at initiative "
  _ginitiative <- Lex.decimal
  space
  let _gid = 0
  pure $ Group { .. }

pTeam :: Parser [Group]
pTeam = try $ do
  team <- takeWhile1P Nothing (liftA2 (||) isAlpha (== ' '))
  ":"
  space
  Text.Megaparsec.many (pGroup team)

pInput :: Parser [[Group]]
pInput = pTeam `sepEndBy1` space

effectivePower :: Group -> Int
effectivePower grp = (grp ^. gdmg) * (grp ^. gunits)

dmgAgainst :: Group -> Group -> Int
dmgAgainst attacker defender
  | attacker ^. gdmgtype `elem` (defender ^. gimmune)
  = 0
  | attacker ^. gdmgtype `elem` (defender ^. gweak)
  = (2 * effectivePower attacker)
  | otherwise
  = effectivePower attacker

targetSelectionPriority :: Group -> (Int, Int)
targetSelectionPriority grp =
  (negate $ effectivePower grp, negate $ grp ^. ginitiative)

data AttackPlan = AttackPlan Int Int deriving Show

selectTarget :: Group -> Map Int Group -> Maybe AttackPlan
selectTarget self others
  | null enemies = Nothing
  | dmgAgainst self bestTarget == 0 = Nothing
  | otherwise = Just $ AttackPlan (self ^. gid) $ view gid $ bestTarget
 where
  enemies    = Map.elems others & filter (\x -> x ^. gteam /= self ^. gteam)
  bestTarget = maximumOn
    (\e -> (dmgAgainst self e, effectivePower e, _ginitiative e))
    enemies

selectTargets :: Map Int Group -> [AttackPlan]
selectTargets grps = catMaybes $ runST $ do
  let attackers = sortOn targetSelectionPriority (Map.elems grps)
  defenders <- newSTRef grps
  forM attackers $ \attacker -> do
    defs <- readSTRef defenders
    let atkpln = selectTarget attacker defs
    case atkpln of
      Nothing                   -> pure ()
      Just (AttackPlan _ defID) -> writeSTRef defenders $ Map.delete defID defs
    pure atkpln

runAttack :: Map Int Group -> AttackPlan -> Map Int Group
runAttack grps (AttackPlan atk def) | attackerAlive = grps & at def .~ victim
                                    | otherwise     = grps
 where
  attackerAlive = isJust $ grps Map.!? atk
  victim        = do
    attacker <- grps Map.!? atk
    defender <- grps Map.!? def
    let damage         = dmgAgainst attacker defender
        hp             = defender ^. ghp
        kills          = damage `div` hp
        defenderUnits  = defender ^. gunits
        remainingUnits = defenderUnits - kills
    guard $ remainingUnits > 0
    pure $ defender & gunits .~ remainingUnits

battle :: Map Int Group -> Map Int Group
battle grps | multipleTeamsAlive = battle survivors
            | otherwise          = survivors
 where
  selections =
    selectTargets grps
      & sortOn (\(AttackPlan a _d) -> grps ^?! ix a . ginitiative)
      & reverse
  survivors = foldl' runAttack grps selections
  multipleTeamsAlive =
    (> 1) $ Set.size $ Set.fromList $ view gteam <$> Map.elems grps

headCount :: Map Int Group -> Int
headCount = Map.elems >>> map (view gunits) >>> sum

main :: IO ()
main = do
  stuff <- getContents
  input <- case parse pInput "" stuff of
    Left  boo -> die $ strConv Protolude.Strict $ errorBundlePretty boo
    Right joy -> pure joy
  let setid id grp = grp & gid .~ id
  let groups = Map.fromList $ (\g -> (g ^. gid, g)) <$> zipWith
        setid
        [0 ..]
        (mconcat input)
  let end = battle $ groups
  pPrint $ headCount end
  forM_ [0 ..] $ \boost -> do
    putStr $ (show boost <> " ... " :: [Char])
    let applyBoost = \g -> case view gteam g of
          "Infection"     -> g
          "Immune System" -> g & gdmg %~ (+ boost)
        boostedAnimals = applyBoost <$> groups
    rigged <- timeout 1000000 $ do
      pure $! battle boostedAnimals
    case rigged of
      Nothing  -> putText "timeout"
      Just res -> do
        let team = head (view gteam <$> Map.elems res)
        when (team == Just "Immune System") $ do
          pPrint $ headCount res
          exitSuccess
        case team of
          Just t  -> putText t
          Nothing -> print team
