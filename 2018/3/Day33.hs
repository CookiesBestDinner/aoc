{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text

main :: IO ()
main = do input <- TIO.getContents
          let ls = T.lines input
          print "day 3 part 1"
          let claims = fromRight $ parseOnly (pClaim `sepBy` char '\n') input
          putStrLn $ unlines $ map show claims

data Claim = Claim { n      :: Integer
                   , col    :: Integer
                   , row    :: Integer
                   , width  :: Integer
                   , height :: Integer
                   } deriving (Show)

test :: Text
test = "#11 @ 569,720: 28x29"

pClaim = Claim <$ char '#'
  <*> decimal
  <* string " @ "
  <*> decimal
  <* char ','
  <*> decimal
  <* string ": "
  <*> decimal
  <* char 'x'
  <*> decimal

fromRight (Right x) = x
