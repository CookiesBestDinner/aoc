{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day13
  ( main
  )
where

import           Common                         ( executeParser )
import           Intcode

import           Conduit
import           Control.Arrow
import           Control.Lens
import           Data.List.Extra                ( chunksOf )
import qualified Data.Map.Strict               as Map
import qualified Prelude
import           Protolude
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                )

main :: Text -> IO ()
main indata = do
  savefile <- Prelude.readFile "input/day13_recorded_input"
  game     <- executeParser parseComp indata
  let part1 =
        let canvas = Map.fromList (paintUpdate <$> drawThis)
            drawThis =
                runConduitPure (pure () .| run game .| sinkList) & chunksOf 3
            paintUpdate [x, y, t] = ((x, y), t)
            paintUpdate badChunk  = panic (show badChunk)
        in  canvas & Map.elems & filter (== 2) & length
  recordedKeys <- newMVar []
  let wireTap = await >>= \case
        Just key -> do
          old <- liftIO $ takeMVar recordedKeys
          liftIO $ putMVar recordedKeys (key : old)
          yield key
          wireTap
        Nothing -> pure ()
  let source = do
        yieldMany savefile
        liftIO $ hSetBuffering stdin NoBuffering
        (liftIO Prelude.getContents >>= yieldMany) .| wireTap
  let insertQuarters = mem . at 0 ?~ 2
  runConduit
    $  source
    .| readKey
    .| run (insertQuarters game)
    .| updateScreen 0 Map.empty
  recorded <- takeMVar recordedKeys
  unless (null recorded) $ do
    putText "recorded keys: "
    recorded & reverse & print
  putText $ "Part 1 was: " <> show part1

readKey :: ConduitT Char Int IO ()
readKey = do
  key <- await
  case key of
    Just 'a' -> yield (-1)
    Just 'o' -> yield 0
    Just 'e' -> yield 1
    _        -> pure ()
  when (isJust key) readKey

updateScreen :: MonadIO m => Int -> Map (Int, Int) Int -> ConduitT Int c m ()
updateScreen score canvas = takeC 3 .| sinkList >>= \case
  [x, y, t] -> do
    let (score', canvas') | (x, y) == (-1, 0) = (t, canvas)
                          | otherwise = (score, Map.insert (x, y) t canvas)
    liftIO $ drawCanvas canvas'
    liftIO $ putText $ "Score: " <> show score'
    updateScreen score' canvas'
  _ -> pure ()

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
  getTile = \case
    0 -> ' '
    1 -> '█'
    2 -> '#'
    3 -> '='
    4 -> 'o'
    o -> panic $ show o
