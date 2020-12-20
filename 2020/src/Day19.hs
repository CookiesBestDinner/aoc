module Day19 where

import qualified Data.Map                   as Map
import           Prelude                    (String)
import           Protolude                  hiding (many, try)
import           Text.Megaparsec            hiding (Token)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

-- import           Parsing

type Parser = Parsec Void String

data Rule
  = RChar (Parser String)
  | ROR (Parser String) (Parser String)

charsP :: Parser (Parser String)
charsP = do
  "\""
  s <- takeWhile1P Nothing (/= '"')
  "\""
  pure $ string s

numsP :: Parser (Map Int (Parser String) -> Parser String)
numsP = do
  xs <- decimal `sepBy1` " "
  pure $ \lookup -> do
    mconcat <$> traverse (lookup Map.!) xs

ruleOrP :: Parser (Map Int (Parser String) -> Parser String)
ruleOrP = do
  as :: [Int] <- decimal `sepEndBy1` " "
  "| "
  bs :: [Int] <- decimal `sepBy1` " "
  pure $ \lookup -> do
    let option1 :: Parser String = mconcat <$> traverse (lookup Map.!) as
    let option2 :: Parser String = mconcat <$> traverse (lookup Map.!) bs
    try option1 <|> option2

ruleP :: Parser (Int, Map Int (Parser String) -> Parser String)
ruleP = do
  name <- decimal
  ": "
  rule <- try numsP <|> try ruleOrP <|> (const <$> charsP)
  pure (name, rule)

inputP :: Parser ([(Int, Map Int (Parser String) -> Parser String)], [String])
inputP = do
  rules <- ruleP `sepEndBy` eol
  eol
  msgs <- takeWhile1P Nothing (/= '\n') `sepEndBy` eol
  space
  eof
  pure (rules, msgs)

main :: Text -> IO ()
main input = do
  (rules, msgs) <- case parse inputP "" (toS input) of
    Left  boo -> putStrLn (errorBundlePretty boo) >> exitFailure
    Right yay -> pure yay
  let rulemap :: Map Int (Parser String) =
        Map.fromList (rules <&> map ($ rulemap))
  let parseMay s p = case parse (p <* eof) "" s of
        Left  _   -> Nothing
        Right yay -> Just yay
  let matchesRule0 s = isJust $ parseMay s (rulemap Map.! 0)
  print $ filter matchesRule0 msgs & length
