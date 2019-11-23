{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import           Control.Monad.State
import           Data.Map.Strict                          ( (!) )
import qualified Data.Map.Strict               as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Protolude
import           Text.Megaparsec                   hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.Text                     as Text

type Parser = Parsec Void Text

pRelation :: Parser (Int, [Int])
pRelation = do
  me <- L.decimal
  " <-> "
  them <- L.decimal `sepBy1` ", "
  return (me, them)

pRelations :: Parser (Map.Map Int ([Int]))
pRelations = do
  pairs <- (pRelation `sepEndBy1` newline) <* eof
  return $ Map.fromList pairs

main = do
  input <- getContents
  case part1 input of
    Left  boo -> putStr boo
    Right res -> print res

part1 input = do
  relationmap <- case parse pRelations "" input of
    Right woohoo  -> Right woohoo
    Left  boooooo -> Left $ traceId $ Text.pack $ errorBundlePretty boooooo
  let seen = evalState (findAll relationmap)
                       (SearchState (Set.singleton 0) (Set.singleton 0))
  return $ Set.size seen

part2 input = do
  relationmap <- case parse pRelations "" input of
    Right woohoo  -> Right woohoo
    Left  boooooo -> Left $ traceId $ Text.pack $ errorBundlePretty boooooo
  let getGroup i = evalState
        (findAll relationmap)
        (SearchState (Set.singleton i) (Set.singleton i))
      startPoints = Map.keys relationmap
  startPoints <&> getGroup & Set.fromList & Set.size & return

data SearchState =
  SearchState
    { seen  :: Set Int
    , queue :: Set Int
    }

findAll :: Map Int ([Int]) -> State (SearchState) (Set Int)
findAll rels = do
  s <- get
  if Set.null (queue s)
    then return (seen s)
    else do
      -- pop.
      let here = Set.findMin (queue s)
      -- get all nearby that are new
      let newPpl =
            Set.fromList
              [ friend
              | friend <- rels ! here
              , friend `Set.notMember` (seen s)
              ]
      -- add to seen
      let newSeen = (seen s) `Set.union` newPpl
      -- update queue (remove pop, add new ones)
      let newQ    = Set.delete here (queue s `Set.union` newPpl)
      put $ SearchState { seen = newSeen, queue = newQ }
      findAll rels
