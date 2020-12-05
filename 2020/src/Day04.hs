module Day04 where

import           Control.Monad        (fail)
import           Data.Char
import           Data.List            ((\\))
import qualified Data.List.NonEmpty   as NEL
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Parsing

main :: Text -> IO ()
main input = do
  let passports = Text.splitOn "\n\n" input
  parsedPassports <- traverse (parse' passportParser) passports
  getArgs >>= \args -> unless (null args) (showandtell passports)
  print $ filter isValidPassport parsedPassports & length
  print $ length $ rights $ parse passportParser2 "" <$> passports

showandtell passports = do
  forM_ passports $ \passport -> do
    putText "--------------------INPUT----------------------"
    putText passport
    case parse passportParser2 "" passport of
      Left boo -> do
        putText "--------------------FAIL-----------------------"
        putStr $ errorBundlePretty boo
      _ -> putText "--------------------SUCCESS--------------------"

-- part 1: read key:value pairs -> check the keys
passportParser :: Parser [([Char], [Char])]
passportParser = pField `sepEndBy1` space <* eof
 where
  pField = do
    key <- takeWhile1P Nothing (/= ':')
    ":"
    value <- takeWhile1P Nothing (not . isSpace)
    pure (toS key, toS value)

isValidPassport fields = all isPresent requiredFields
 where
  presentFields  = fst <$> fields
  isPresent      = (`elem` presentFields)
  requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- part 2: parse the values
digit :: Parser Int
digit = digitToInt <$> oneOf ("0123456789" :: [Char])

numFieldParser :: Int -> Int -> Int -> Parser ()
numFieldParser len lo hi = do
  digits <- replicateM len digit
  let n = foldl' (\acc d -> acc * 10 + d) 0 digits
  unless (n >= lo && n <= hi) $ do
    fail $ show n <> " is not in range [" <> show lo <> ", " <> show hi <> "]"

pBYR = "byr:" >> numFieldParser 4 1920 2002 >> pure "byr"
pIYR = "iyr:" >> numFieldParser 4 2010 2020 >> pure "iyr"
pEYR = "eyr:" >> numFieldParser 4 2020 2030 >> pure "eyr"
pHCL =
  "hcl:#" >> replicateM 6 (oneOf ("0123456789abcdef" :: [Char])) >> pure "hcl"
pECL =
  "ecl:" >> choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] >> pure
    "ecl"
pPID = "pid:" >> replicateM 9 digit >> pure "pid"
pCID = "cid:" >> takeWhileP Nothing (not . isSpace) >> pure "cid"
pHGT = do
  "hgt:"
  n    <- number
  unit <- "cm" <|> "in"
  when (unit == "in") $ do
    unless (n >= 59 && n <= 76) $ do
      fail $ show n <> " inches is not in range [59, 76]"
  when (unit == "cm") $ do
    unless (n >= 150 && n <= 193) $ do
      fail $ show n <> " cm is not in range [150, 193]"
  pure "hgt"

passportParser2 :: Parser ()
passportParser2 = do
  fields <- choice [pBYR, pIYR, pEYR, pHGT, pHCL, pECL, pPID, pCID]
    `sepEndBy` oneOf (" \n" :: [Char])
  let required = words "byr iyr eyr hgt hcl ecl pid"
      missing  = required \\ fields
  unless (null missing) $ do
    failure Nothing (Set.fromList (Label . NEL.fromList . show <$> missing))
