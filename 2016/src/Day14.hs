{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import           Control.Arrow
import qualified Crypto.Hash                   as C
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as BS16
import           Data.List                                ( iterate' )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Protolude

main :: IO ()
main = do
  let part1 = md5Hex <$> (input <>) <$> show <$> [0 :: Int ..]
  let part2 = stretch <$> part1
  part1 & find64th & print
  part2 & find64th & print

md5Hex :: BS.ByteString -> Text
md5Hex bytes = show digest
 where
  digest :: C.Digest C.MD5
  digest = C.hash bytes

-- presumably faster by not converting <-> Text but I haven't tested
hexBSMD5 :: ByteString -> ByteString
hexBSMD5 bs = MD5.update MD5.init bs & MD5.finalize & BS16.encode

stretch :: Text -> Text
stretch =
  E.encodeUtf8
    >>> iterate' hexBSMD5
    >>> take 2017
    >>> lastDef (panic "please send help")
    >>> E.decodeUtf8

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
  triple <- h & T.group & filter (T.length >>> (>= 3)) & head
  digit  <- triple & T.unpack & head
  (quinMap Map.! digit)
    & takeWhile (\(i', _q) -> i' <= i + 1000)
    & dropWhile (\(i', _q) -> i' <= i)
    & head

input :: BS.ByteString
-- input = E.encodeUtf8 "abc"
input = E.encodeUtf8 "ihaygndm"

find64th :: [Text] -> Maybe (Int, Text)
find64th hashes =
  hashes
    & zip [0 ..]
    & filter (isValidHash (quintupleMap hashes))
    & take 64
    & lastMay
