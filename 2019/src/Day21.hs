{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day21 where

import           Common                     (executeParser)
import           Intcode

import           Conduit
import           Protolude
import           Text.InterpolatedString.QM

main :: Text -> IO ()
main indata = do
  vm <- executeParser parseComp indata
  -- if there's a whole in the next three,
  -- and there's a landing spot
  -- didn't really expect that to work but that's part 1,
  -- part2 breaks the naiive approach
  -- ...and then I'm not really sure what to do.
  let script :: [Char] = (<> "\n") [qnb|
        NOT A J
        NOT B T
        OR T J
        NOT C T
        OR T J
        OR T J
        AND D J
        WALK
        |]
  let encode = await >>= \case
        Nothing -> pure ()
        Just n -> (if n < 256 then yield (toS [chr n]) else yield (show n)) >> encode
  runConduit $ yieldMany script .| mapC ord .| run vm .| encode .| stdoutC
  putText ""
