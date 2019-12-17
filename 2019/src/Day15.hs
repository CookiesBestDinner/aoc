{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import           Common                         ( executeParser )
import           Intcode

import           Conduit
import           Control.Arrow
import           Control.Lens
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import qualified Prelude
import           Protolude
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                )

data Dir = DUp|DDown|DLeft|DRight deriving (Eq, Show)

main :: Text -> IO ()
main indata = do
  vm <- executeParser parseComp indata
  let searchThis = SearchState 0 vm (0, 0) Set.empty
      searchRes  = search (Set.singleton searchThis)
  print (_steps searchRes)
  let vmAtOx      = _comp searchRes
  let searchThis2 = SearchState 0 vmAtOx (0, 0) Set.empty
  print $ searchUntilNothing (Set.singleton searchThis2)

data SearchState = SearchState
  { _steps :: Int
  , _comp :: Comp
  , _loc :: (Int, Int)
  , _visited :: Set (Int, Int)
  } deriving (Eq, Ord, Show)

-- | run search until all searches die (and it's not actually correct, because
-- it finds the single longest possible path that runs into a dead end, it
-- isn't a flood fill, yet somehow still correct answer for my input)
-- an example where this is wrong is if there's a loop far away from the
-- oxygen, and reaching the farthest part of the loop takes X time, but the
-- longest path will be able to continue until it closes the loop, therefore
-- finding a result of X+some extra
searchUntilNothing :: Set SearchState -> Int
searchUntilNothing queue = if null nextQueue
  then _steps here
  else searchUntilNothing nextQueue
 where
  (here, rest) = Set.deleteFindMin queue
  nexts        = catMaybes $ moveNSee here <$> [DUp, DDown, DLeft, DRight]
  nextQueue    = foldr Set.insert rest (rights nexts)

-- | run search until oxygen is found (bfs)
search :: Set SearchState -> SearchState
search queue = case lefts nexts of
  (yay : _) -> yay
  []        -> search nextQueue
 where
  (here, rest) = Set.deleteFindMin queue
  nexts        = catMaybes $ moveNSee here <$> [DUp, DDown, DLeft, DRight]
  nextQueue    = foldr Set.insert rest (rights nexts)

-- | results in Nothing if hitting wall or previously visited location
-- Left when finding oxygen
-- Right for continue searching
moveNSee :: SearchState -> Dir -> Maybe (Either SearchState SearchState)
moveNSee s d
  | output == 0 = Nothing
  | loc `Set.member` _visited s = Nothing
  | output == 2 = Just $ Left $ SearchState (_steps s + 1) comp loc visited
  | otherwise = Just $ Right $ SearchState (_steps s + 1) comp loc visited
 where
  input = yieldMany $ case d of
    DUp    -> [1]
    DDown  -> [2]
    DLeft  -> [3]
    DRight -> [4]
  [(output, comp)] =
    runConduitPure $ input .| run' (_comp s) .| takeC 1 .| sinkList
  (x, y) = _loc s
  loc    = case d of
    DUp    -> (x, y + 1)
    DDown  -> (x, y - 1)
    DLeft  -> (x - 1, y)
    DRight -> (x + 1, y)
  visited = Set.insert loc (_visited s)






-- | not used, not useful.
-- Don't get lost in there.
playTheGameForNoReason :: Comp -> IO ()
playTheGameForNoReason vm = do
  keypressed <- newMVar DUp
  let source = do
        let press :: (Int, Dir) -> IO Int
            press (i, d) = do
              takeMVar keypressed
              putMVar keypressed d
              return i
        liftIO $ hSetBuffering stdin NoBuffering
        inputs <- liftIO Prelude.getContents
        yieldMany (inputs & filter (`elem` ("aoe," :: [Char])))
          .| mapC readCmd
          .| mapMC press
  let tracker = go (0, 0) Map.empty
      go (x, y) meep = do
        liftIO $ display meep (x, y)
        event <- fromMaybe undefined <$> await
        dir   <- liftIO $ takeMVar keypressed
        liftIO $ putMVar keypressed dir
        let loc = case dir of
              DUp    -> (x, y + 1)
              DDown  -> (x, y - 1)
              DLeft  -> (x - 1, y)
              DRight -> (x + 1, y)
        case event of
          0 -> do
            let mee = Map.insert loc '#' meep
            go (x, y) mee
          1 -> do
            let mee = Map.insert loc '.' meep
            go loc mee
          2 -> do
            let mee = Map.insert loc 'X' meep
            go loc mee
  runConduit (source .| run vm .| tracker)
 where
  readCmd :: Char -> (Int, Dir)
  readCmd ',' = (1, DUp)
  readCmd 'o' = (2, DDown)
  readCmd 'a' = (3, DLeft)
  readCmd 'e' = (4, DRight)
  display :: Map (Int, Int) Char -> (Int, Int) -> IO ()
  display canvas loc
    | Map.null canvas = return ()
    | otherwise = do
      rows & mapM_ putStrLn
      putText ""
      putText (show (Map.size canvas))
      putText (show (length rows))
   where
    (lox, hix) = canvas & Map.keys <&> fst & (minimum &&& maximum)
    (loy, hiy) = canvas & Map.keys <&> snd & (minimum &&& maximum)
    rows =
      [ [ if (x, y) == loc then '^' else cell
        | x <- [lox .. hix]
        , let cell = Map.findWithDefault ' ' (x, y) canvas
        ]
      | y <- [hiy, hiy - 1 .. loy]
      ]
