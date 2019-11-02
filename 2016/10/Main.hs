{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude                         hiding ( from
                                                          , many
                                                          )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified Data.Map.Strict               as Map

type Parser = Parsec Void Text

pInstructions :: Parser [Either Init Sort]
pInstructions = ((Right <$> pSort) <|> (Left <$> pInit)) `sepEndBy` newline <* eof

-- | bot 147 gives low to bot 67 and high to bot 71
--   bot 68 gives low to output 18 and high to output 5
pSort :: Parser Sort
pSort = do
  _           <- "bot "
  from        <- decimal
  _           <- " gives low to "
  lowBinType  <- "bot " $> Bot <|> "output " $> Output
  low         <- decimal
  _           <- " and high to "
  highBinType <- "bot " $> Bot <|> "output " $> Output
  high        <- decimal
  pure $ Sort { .. }

-- | value 73 goes to bot 177
pInit :: Parser Init
pInit = do
  _     <- "value "
  value <- decimal
  _     <- " goes to bot "
  bot   <- decimal
  pure $ Init { .. }

data Init =
  Init
    { bot   :: Int
    , value :: Int
    } deriving Show

data Sort =
  Sort
    { from  :: Int
    , high :: Int
    , highBinType :: BinType
    , low  :: Int
    , lowBinType :: BinType
    } deriving Show

data BinType = Bot | Output deriving Show

data FactoryState =
  FactoryState
    { bots :: Map Int [Int]
    , outputs :: Map Int [Int]
    , comparisons :: Map (Int, Int) Int
    , active :: [Int]
    , programming :: Map Int Sort
    } deriving Show

buildFactory :: [Init] -> [Sort] -> FactoryState
buildFactory setup sorts = FactoryState { .. }
 where
  outputs     = Map.empty
  comparisons = Map.empty
  bots        = Map.fromListWith (<>) $ do
    Init bot value <- setup
    pure (bot, [value])
  active = do
    (botId, chips) <- Map.assocs bots
    guard $ length chips == 2
    pure botId
  programming = Map.fromList $ do
    code <- sorts
    let id = from code
    pure (id, code)


factoryStep :: FactoryState -> FactoryState
factoryStep f = applied { active = newActives }
 where
  actives    = active f
  applied    = foldl' monstrosity f actives
  newActives = do
    (botid, chips) <- Map.assocs (bots applied)
    guard $ length chips == 2
    pure botid

monstrosity factory activeBot =
  factory & emptySelf & addHigh & addLow & addComp
 where
  chips    = Map.findWithDefault [] activeBot (bots factory)
  srt      = programming factory Map.! activeBot
  emptySelf f = f { bots = Map.insert activeBot [] (bots f) }
  addHigh f = case highBinType srt of
    Output ->
      let m    = outputs f
          lst  = Map.findWithDefault [] (high srt) m
          lst' = (maximum chips : lst)
          m'   = Map.insert (high srt) lst' m
      in  f { outputs = m' }
    Bot ->
      let m    = bots f
          lst  = Map.findWithDefault [] (high srt) m
          lst' = (maximum chips : lst)
          m'   = Map.insert (high srt) lst' m
      in  f { bots = m' }
  addLow f = case lowBinType srt of
    Output ->
      let m    = outputs f
          lst  = Map.findWithDefault [] (low srt) m
          lst' = (minimum chips : lst)
          m'   = Map.insert (low srt) lst' m
      in  f { outputs = m' }
    Bot ->
      let m    = bots f
          lst  = Map.findWithDefault [] (low srt) m
          lst' = (minimum chips : lst)
          m'   = Map.insert (low srt) lst' m
      in  f { bots = m' }
  addComp f =
    let key = (minimum chips, maximum chips)
        m   = comparisons f
    in  f { comparisons = Map.insert key activeBot m }

runFactory f | null (active f) = f
             | otherwise       = runFactory $ factoryStep f

main = do
  input        <- getContents
  instructions <- case parse pInstructions "" input of
    Right things -> pure things
    Left  bundle -> do
      putStr (errorBundlePretty bundle)
      exitFailure
  let setup           = lefts instructions
      sorts           = rights instructions
      factory         = buildFactory setup sorts
      finishedFactory = runFactory factory
  putStrLn ("hello world" :: Text)
  print (comparisons finishedFactory)
  putStrLn ("------------" :: Text)
  print (outputs finishedFactory)
