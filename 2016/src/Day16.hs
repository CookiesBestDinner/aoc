{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day16 where
import qualified Data.Text as Text
import           Protolude

main :: IO ()
main =
  let input = "10111011111001111"
      disk  = 35651584
      -- so this should be like 35MB right? make that 100MB for good measure
      -- I'm using like 10 times that. which is silly!
      -- I expect python would give low memory usage and good performance
      -- without having to think about it.
      -- [Char] used 10GB .. lol.
      -- yeah, I could bring out mutable vector and do it all in-place
      -- whatever.
  in  input & dragonTo disk & checkSum & print

flips :: Text -> Text
flips = Text.reverse . Text.map flipch where
  flipch '1' = '0'
  flipch '0' = '1'
  flipch _   = panic "no."

dragonTo :: Int -> Text -> Text
dragonTo n s | Text.length s >= n = Text.take n s
             | otherwise          = dragonTo n (dragonStep s)
  where dragonStep a = a <> "0" <> flips a

checkSum :: Text -> Text
checkSum s | odd $ Text.length s = s
           | otherwise = Text.chunksOf 2 s <&> match & mconcat & checkSum
 where
  match "11" = "1"
  match "00" = "1"
  match _    = "0"
