{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import qualified Crypto.Hash                   as C
import qualified Crypto.Hash.MD5                   as MD5
import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Data.List                                ( iterate' )
import           Protolude
import           Text.Show.Pretty

md5Hex :: BS.ByteString -> Text
md5Hex bytes = show digest
 where
  digest :: C.Digest C.MD5
  digest = C.hash bytes

hexdigits :: [Char]
hexdigits = ['0' .. '9'] <> ['a' .. 'f']

quintupleMap :: [Text] -> Map Char [(Int, Text)]
quintupleMap hashes = Map.fromList $ do
  key <- hexdigits
  let quintuple = replicate 5 key & T.pack
  let quins = hashes & zip [0 ..] & filter (snd >>> T.isInfixOf quintuple)
  pure (key, quins)

isValidHash :: Map Char [(Int, Text)] -> (Int, Text) -> Bool
isValidHash quinMap (i, h) = not . null $ do
  -- has a triple
  triple <- h & T.group & filter (T.length >>> (== 3)) & head
  -- what is the digit of that triple
  digit  <- triple & T.unpack & head
  (quinMap Map.! digit)
    & takeWhile (\(i', _q) -> i' <= i + 1000)
    & dropWhile (\(i', _q) -> i' <= i)
    & head

input :: BS.ByteString
input = E.encodeUtf8 "ihaygndm"
-- input = E.encodeUtf8 "abc"

main :: IO ()
main = do
  let rawhashes = md5Hex <$> ((input <>) . show) <$> [0 :: Int ..]
  let quinMap   = quintupleMap rawhashes
  rawhashes
    & zip [0 ..]
    & filter (isValidHash quinMap)
    & take 64
    & zip [1 ..]
    & drop 54
    & pPrint
  putText "So err.. It's one of those. I think. Hold on a few mins for part b"
  let rawhashes' = stretch <$> md5Hex <$> ((input <>) . show) <$> [0 :: Int ..]
  let quinMap'   = quintupleMap rawhashes'
  rawhashes'
    & zip [0 ..]
    & filter (isValidHash quinMap')
    & take 64
    & zip [1 ..]
    & drop 54
    & pPrint

stretch :: Text -> Text
stretch = E.encodeUtf8 >>> iterate' hexBSMD5 >>> take 2017 >>> lastDef
  (panic "please send help") >>> E.decodeUtf8

hexBSMD5 :: ByteString -> ByteString
hexBSMD5 bs = MD5.update MD5.init bs & MD5.finalize & BS16.encode

-- my 62nd one-time pad key was the one that is actually the 64th
-- (did binary search with submissions since it says high/low and I have some
-- good candidates)
-- so it seems like there are two hashes that I am not finding.

-- same deal for part 2. the 61st key was the winner
-- .. oh yeah and it takes 2½ minutes to run.
