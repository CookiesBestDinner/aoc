-- yo dawg, i heard u like interpreters
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day17 where

import           Common                         ( executeParser )
import           Intcode

import           Conduit
import           Control.Lens
import qualified Data.Map.Strict               as Map
import qualified Prelude
import qualified Data.Text                     as T
import           Protolude
import           Text.InterpolatedString.QM

data Dir
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Eq, Show)

main :: Text -> IO ()
main indata = do
  vm <- executeParser parseComp indata
  let grid = runConduitPure $ pure () .| run vm .| mapC chr .| sinkList
  let gridMap = Map.fromList $ do
        (y, row ) <- zip [0 ..] (Prelude.lines grid)
        (x, cell) <- zip [0 ..] row
        return ((x, y), cell)
  let isIntersect :: (Int, Int) -> Bool
      isIntersect (x, y) = around <&> deref & filter (== '#') & length & (== 5)
       where
        around = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x, y)]
        deref coord = Map.findWithDefault '.' coord gridMap
  gridMap & Map.keys & filter isIntersect <&> uncurry (*) & sum & print
  let roombaProgram :: [Char] = (<> "\n") [qnb|
        A,B,A,C,A,B,C,B,C,B
        R,10,R,10,R,6,R,4
        R,10,R,10,L,4
        R,4,L,4,L,10,L,10
        n
        |]
  let wakeUp = mem . at 0 ?~ 2
  runConduit
    $  yieldMany roombaProgram
    .| mapC ord
    .| run (wakeUp vm)
    .| mapC (\i -> if i < 256 then T.singleton (chr i) else show i)
    .| encodeUtf8C
    .| stdoutC
  putText ""
