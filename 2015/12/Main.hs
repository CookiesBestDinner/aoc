{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text                     ( signed
                                                          , decimal
                                                          , parseOnly
                                                          )
import           Control.Arrow
import           Replace.Attoparsec.Text                  ( findAll )
import qualified Data.Text.IO                  as TIO
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TextEncoding
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                              ( rights )
import           System.Exit
import           Data.Function                            ( (&) )
import           Data.Aeson
import           Data.Scientific                          ( floatingOrInteger )
import qualified Data.HashMap.Strict           as HashMap

textToJsonAST :: T.Text -> Value
textToJsonAST t = case decode bytes of
  Just ast -> ast
  Nothing  -> error ("could not parse: " <> T.unpack t)
 where
  bytes' = TextEncoding.encodeUtf8 t
  bytes  = BL.fromStrict bytes'

main = do
  -- part1: a text search is sufficient here, no need to parse the json
  -- ... but I do use parsers for identifying numbers.
  jsontext <- TIO.getContents
  let parseResult = parseOnly findNums jsontext
  numbers <- case parseResult of
    Left err -> do
      putStrLn $ "Error while parsing: " <> err
      exitFailure
    Right ns -> return ns
  print $ numbers & map (T.unpack >>> read :: T.Text -> Integer) & sum
  -- part2: need to look at values in the json objects, so parse it
  let ast = textToJsonAST jsontext
  print $ numberExtractor ast

numberExtractor :: Value -> Integer
numberExtractor (Number n) = case floatingOrInteger n of
  Right n' -> n'
  Left  _  -> error "supposed to only contain integers"
numberExtractor (Object obj) =
  let values = HashMap.elems obj
  in  if "red" `elem` values then 0 else sum $ numberExtractor <$> values
numberExtractor (Array  array) = sum $ numberExtractor <$> array
numberExtractor (String _    ) = 0
numberExtractor (Bool   _    ) = 0
numberExtractor Null           = 0

findNums = rights <$> findAll (signed decimal)
