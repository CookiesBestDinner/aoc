{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

pSOMETHING :: Parser ()
pSOMETHING = do
  undefined

getInput :: Text -> IO ()
getInput input = case parse pSOMETHING "" input of
  Left boo -> do
    putStrLn $ errorBundlePretty boo
    exitFailure
  Right yay -> return yay

main :: IO ()
main = do
  input <- getContents
  putText "Herro world!"
