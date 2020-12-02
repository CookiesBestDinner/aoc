{-# LANGUAGE RecordWildCards #-}
module Day02 where

import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Parsing

data Password
  = Password
      { lo  :: Int
      , hi  :: Int
      , ch  :: Char
      , str :: [Char]
      }
  deriving (Show)

pwParser :: Parser Password
pwParser = do
  lo <- number
  "-"
  hi <- number
  space
  ch <- anySingle
  ":"
  space
  str <- toS <$> takeWhileP Nothing (/= '\n')
  pure $ Password { .. }

valid :: Password -> Bool
valid password = inrange
 where
  count :: Int = filter (== ch password) (str password) & length
  inrange      = count >= lo password && count <= hi password

validpart2 :: Password -> Bool
validpart2 password = ab == expect
 where
  get n = take n (str password) & drop (n - 1) & filter (== ch password)
  ab     = sort [get (lo password), get (hi password)]
  expect = [[], [ch password]]

main :: Text -> IO ()
main input = do
  passwords <- parse' (pwParser `sepEndBy1` space) input
  print $ filter valid passwords & length
  print $ filter validpart2 passwords & length
