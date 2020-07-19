{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day17 where

import qualified Crypto.Hash     as Crypto
import qualified Data.ByteString as BS
import           Data.List.Extra (maximumOn, minimumOn)
import           Data.Sequence   (Seq ((:<|)))
import qualified Data.Sequence   as Seq
import           Protolude       hiding (Down, Left, Right, option)

data Dir = U | D | L | R deriving Show

width, height :: Int
width = 4
height = 4

main :: IO ()
main = do
  let key   = "ulqzkmiv"
      paths = search $ Seq.singleton (key, 0, 0)
      part1 = minimumOn BS.length paths & BS.drop (BS.length key)
      part2 = maximumOn BS.length paths & BS.drop (BS.length key) & BS.length
  putStrLn part1
  print part2

openDoors :: BS.ByteString -> [Dir]
openDoors bytes = case isOpen <$> show digest of
  (up : down : left : right : _) ->
    [(U, up), (D, down), (L, left), (R, right)] & filter snd <&> fst
  _ -> panic "pretty sure an md5 hash has at least 4 bytes"
 where
  digest :: Crypto.Digest Crypto.MD5
  digest = Crypto.hash bytes
  isOpen = (`elem` ("bcdef" :: [Char]))

-- bfs, but finds all so dfs would be fine too
-- each invocation pops one search state from the front of the queue
-- requeues all the ways to follow up from that
search :: Seq (BS.ByteString, Int, Int) -> [BS.ByteString]
search Seq.Empty = []
search ((path, x, y) :<| xs) =
  let candidates = filter withinGrid $ openDoors path <&> \case
        U -> (path <> show U, x, y - 1)
        D -> (path <> show D, x, y + 1)
        R -> (path <> show R, x + 1, y)
        L -> (path <> show L, x - 1, y)
  in  if (x, y) == (width - 1, height - 1)
        then path : search xs
        else search (xs <> Seq.fromList candidates)

withinGrid :: (a, Int, Int) -> Bool
withinGrid (_, x, y) = x >= 0 && x < width && y >= 0 && y < height
