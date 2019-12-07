{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day07
  ( main
  )
where

import           Common

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Protolude                         hiding ( zero )
import           Text.Megaparsec
import           Text.Megaparsec.Char.Lexer

data Comp =
  Comp
    { _pc     :: Int
    , _input  :: [Int]
    , _output :: [Int]
    , _mem    :: Map.Map Int Int
    }
  deriving (Show)

makeLenses ''Comp

pInput :: Parser [Int]
pInput = signed (pure ()) decimal `sepBy1` ","

main :: Text -> IO ()
main indata = do
  program <- executeParser pInput indata
  let initialMem = Map.fromList $ zip [0 ..] program
  let zero       = Comp 0 [] [0] initialMem
  let candidates = permutations [0 .. 4]
  let results =
        (configure zero initialMem <$> candidates) <&> _output & maximum
  results & print
  part2 program

part2 :: [Int] -> IO ()
part2 program = do
  let initialMem = Map.fromList $ zip [0 ..] program
  let zero       = Comp 0 [] [] initialMem
  let candidates = permutations [5 .. 9]
  let results =
        (configure zero initialMem <$> candidates) <&> _output & maximum
  let rig [a, b, c, d, e] = Map.fromList $ zip
        [0 :: Int ..]
        [ reload a initialMem zero
        , reload b initialMem zero
        , reload c initialMem zero
        , reload d initialMem zero
        , reload e initialMem zero
        ]
  let dothething inputs = runRig (rig inputs) 0 [0]
  [ dothething phasers & (Map.! 4) & (\c -> (_output c))
    | phasers <- candidates
    ]
    & maximum
    & print

-- | run each one until it blocks repeatedly until there's no output
runRig rig i inbound = if isLoop then rig else continue
 where
  isLoop   = null outbound
  setup    = output .~ [] >>> input %~ (<> inbound)
  executed = (rig Map.! i) & setup & run
  outbound = executed ^. output
  continue = runRig (Map.insert i executed rig) ((i + 1) `mod` 5) outbound

configure :: Comp -> Map.Map Int Int -> [Int] -> Comp
configure zero _         []          = zero
configure zero freshCode (i : nputs) = configure
  (run $ reload i freshCode zero)
  freshCode
  nputs
 where


reload :: Int -> Map Int Int -> Comp -> Comp
reload i freshCode comp =
  comp & output2input & nulloutput & resetCode & extraInput & resetPC
 where
  output2input = input .~ (reverse (comp ^. output))
  nulloutput   = output .~ []
  extraInput   = input %~ (i :)
  resetCode    = mem .~ freshCode
  resetPC      = pc .~ 0

run :: Comp -> Comp
run c = case step c of
  Just c' -> run c'
  Nothing -> c

decodeOp :: Int -> (Int, Int, Int, Int)
decodeOp i =
  ( i `mod` 100
  , (i `div` 100) `mod` 10
  , (i `div` 1000) `mod` 10
  , (i `div` 10000) `mod` 10
  )

step :: Comp -> Maybe Comp
step comp
  | i == 99 = Nothing
  | i == 3 && null (comp ^. input) = Nothing
  | otherwise = Just comp <&> case i of
    1 -> pc %~ (+ 4) >>> mem . at c ?~ ra + rb
    2 -> pc %~ (+ 4) >>> mem . at c ?~ ra * rb
    7 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra < rb)
    8 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra == rb)
    3 ->
      pc %~ (+ 2) >>> mem . at a ?~ comp ^?! input . ix 0 >>> input %~ drop 1
    4 -> pc %~ (+ 2) >>> output %~ (ra :)
    5 -> pc .~ if ra /= 0 then rb else p + 3
    6 -> pc .~ if ra == 0 then rb else p + 3
    _ -> panic $ "bad instruction: " <> show i
 where
  lookup loc = comp ^?! (mem . ix loc)
  (i, ma, mb, mc) = decodeOp (lookup p)
  !assertop       = validOpCode (i, ma, mb, mc)
  p               = comp ^. pc
  a               = lookup (p + 1)
  b               = lookup (p + 2)
  c               = lookup (p + 3)
  ra              = if ma == 0 then lookup a else a
  rb              = if mb == 0 then lookup b else b
  _rc             = if mc == 0 then lookup c else c

-- | sanity checking the opcode, can be removed without affecting output
-- panics (crashes) if the opcode isn't something expected
validOpCode op@(i, ma, mb, mc)
  | i `elem` [1, 2, 7, 8] && ma `elem` [0, 1] && mb `elem` [0, 1] && mc == 0
  = ()
  | i == 3 && ma == 0
  = ()
  | i == 4 && ma `elem` [0, 1]
  = ()
  | i `elem` [5, 6] && ma `elem` [0, 1] && mb `elem` [0, 1]
  = ()
  | i == 99
  = ()
  | otherwise
  = panic $ "bad op: " <> show op
