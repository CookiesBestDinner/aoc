{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Day13
  ( main
  )
where

import           Common                         ( executeParser )
import           System.IO                      ( hSetBuffering
                                                , BufferMode(NoBuffering)
                                                , hClose
                                                )
import qualified Data.Map.Strict               as Map
import           Control.Lens
import           Control.Arrow                  ( (&&&) )
import           Data.List.Extra                ( chunksOf )
import qualified Prelude
import           Day09                          ( Comp(Comp)
                                                , pInput
                                                , run
                                                , mem
                                                )
import           Protolude

main :: Text -> IO ()
main indata = do
  savefile <- Prelude.readFile "input/day13_recorded_input"
  hSetBuffering stdin NoBuffering
  joystick <- Prelude.getContents
  program  <- executeParser pInput indata
  let game     = Comp 0 0 (Map.fromList $ zip [0 ..] program)
  let drawThis = run game [] & chunksOf 3
  let paintUpdate [x, y, t] = ((x, y), t)
      paintUpdate badChunk  = panic (show badChunk)
  let canvas = Map.fromList (paintUpdate <$> drawThis)
  canvas & Map.elems & filter (== 2) & length & print
  let insertQuarters = mem . at 0 ?~ 2
  let gameInputs =
        (savefile <> joystick)
          <&> (\case
                'a' -> Just (-1)
                'o' -> Just 0
                'e' -> Just 1
                _   -> Nothing
              )
          &   catMaybes
  let vmOutput = run (insertQuarters game) gameInputs
  drawFrames vmOutput
  putStr "Part 1 was: "
  canvas & Map.elems & filter (== 2) & length & print
  hClose stdin
  putStrLn "keyboard inputs of this session:"
  mapM_ putStr ((: []) <$> joystick)

drawFrames :: [Int] -> IO ()
drawFrames = go 0 Map.empty
 where
  go :: Int -> Map.Map (Int, Int) Int -> [Int] -> IO ()
  go score canvas (x : y : t : rest) = do
    let canvas' | (x, y) == (-1, 0) = canvas
                | otherwise         = Map.insert (x, y) t canvas
    let score' | (x, y) == (-1, 0) = t
               | otherwise         = score
    drawCanvas canvas'
    putStrLn $ "Score: " <> show score
    go score' canvas' rest
  go _ _ _ = return ()

drawCanvas :: Map (Int, Int) Int -> IO ()
drawCanvas canvas = rows & mapM_ putStrLn
 where
  (lox, hix) = canvas & Map.keys <&> fst & (minimum &&& maximum)
  (loy, hiy) = canvas & Map.keys <&> snd & (minimum &&& maximum)
  rows =
    [ [ getTile cell
      | x <- [lox .. hix]
      , let cell = Map.findWithDefault 0 (x, y) canvas
      ]
    | y <- [loy .. hiy]
    ]
  getTile n = case n of
    0 -> ' '
    1 -> '█'
    2 -> '#'
    3 -> '='
    4 -> 'o'
    o -> panic $ show o
