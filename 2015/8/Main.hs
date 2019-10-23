module Main where

import           Control.Arrow
import           Data.Function

main = do
  input <- getContents
  let rows          = lines input
  let withoutQuotes = map (tail >>> init) rows
  let lens = map (bytesToUnits >>> length) withoutQuotes
  let digitalSize   = sum lens
  let paperSize     = concat rows & length
  print $ paperSize - digitalSize
  let encoded = map (encode >>> (\s -> '\x22':s ++ ['\x22'])) rows
  print $ (length $ concat encoded) - paperSize

bytesToUnits ('\\'           : '\\'   : rest) = () : bytesToUnits rest
bytesToUnits ('\\'           : '\x22' : rest) = () : bytesToUnits rest
bytesToUnits ('\\' : 'x' : _ : _      : rest) = () : bytesToUnits rest
bytesToUnits (_                       : rest) = () : bytesToUnits rest
bytesToUnits []                               = []

encode []              = []
encode ('\x22' : rest) = ['\\', '\x22'] ++ encode rest
encode ('\\'   : rest) = ['\\', '\\'] ++ encode rest
encode (ch     : rest) = ch : encode rest
