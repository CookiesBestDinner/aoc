{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Arrow
import           Protolude                         hiding ( hash )
import           Crypto.Hash                              ( MD5
                                                          , Digest
                                                          , hash
                                                          )
import qualified Data.ByteString.Char8         as BSC
import qualified Data.Text                     as T
import qualified Data.Map.Lazy                 as Map

md5Hex :: ByteString -> Text
md5Hex bytes = show digest
 where
  digest :: Digest MD5
  digest = hash bytes

getHash :: BSC.ByteString -> Integer -> Text
getHash door n = md5Hex hashinput
 where
  nBytes    = n & show & BSC.pack
  hashinput = door <> nBytes

main :: IO ()
main = do
  putStr "> "
  doorid <- BSC.getLine
  putStrLn $ "got input: " <> show doorid
  let hashes                = getHash doorid <$> [0 ..]
      hashesStartingW5Zeros = filter (T.pack "00000" `T.isPrefixOf`) hashes
      password              = take 8 $ (`T.index` 5) <$> hashesStartingW5Zeros
  mapM_ print password
  putStrLn password
  -- part 2
  let
    position h = h `T.index` 5
    value h = h `T.index` 6
    validPos h =
      h & position & (`BSC.elem` (BSC.concat $ (BSC.singleton <$> "01234567")))
    p2valids = filter validPos hashesStartingW5Zeros <&> (position &&& value)
    insertE acc (k, v) = do
      let updated = if k `Map.member` acc then acc else Map.insert k v acc
      if Map.size acc == 8 then Left acc else Right updated
    Left pwmap = foldM insertE Map.empty p2valids
    password2  = [ pwmap Map.! k | k <- ['0' .. '7'] ]
  putStrLn password2
