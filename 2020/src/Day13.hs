module Day13 where

import           Control.Arrow
import           Control.Lens
import           Data.List.Extra                          ( minimumOn )
import           Network.URI.Encode                       ( encodeText )
import           Protolude
import           Text.InterpolatedString.QM
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer               ( decimal )

import           Parsing

pInput :: Parser (Int, [Maybe Int])
pInput = do
  let bus = Just <$> decimal <|> "x" $> Nothing
  t <- decimal
  space
  buses <- bus `sepBy1` ","
  space >> eof
  pure (t, buses)

waitTime :: Int -> Int -> Int
waitTime t bus = -(t `mod` (-bus))

main :: Text -> IO ()
main input = do
  (t, buses) <- parse' pInput input
  print $ buses & catMaybes <&> (identity &&& waitTime t) & minimumOn snd
  let constraints :: [(Int, Int)] =
        zip buses [0 ..]
          <&> \case
                (Just b, o) -> Just (b, o)
                _           -> Nothing
          &   catMaybes
  let fmtc (b, o) = [qm|(x + {o}) mod {b} == 0|]
  putStrLn
    $   constraints
    <&> fmtc
    &   intercalate ", "
    &   toS
    &   encodeText
    &   ("https://www.wolframalpha.com/input/?i=" <>)
