module Day02 where

import           Data.Ix         (inRange)
import           Protolude       hiding (many)
import           Text.Megaparsec

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
pwParser =
  Password
    <$> number
    <*  "-"
    <*> number
    <*  " "
    <*> anySingle
    <*  ": "
    <*> restOfLine

valid :: Password -> Bool
valid password = inRange (lo password, hi password) matchcount
  where matchcount = filter (== ch password) (str password) & length

validpart2 :: Password -> Bool
validpart2 password = matchAt lo /= matchAt hi
 where
  matchAt (($ password) -> n) =
    str password & take n & drop (n - 1) & (== [ch password])

main :: Text -> IO ()
main input = do
  passwords <- parse' (many pwParser <* eof) input
  print $ filter valid passwords & length
  print $ filter validpart2 passwords & length
