-- meh.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day12
  ( main
  )
where

import           Common
import           Control.Arrow
import           Control.Lens
import           Data.List.Extra                ( elemIndex
                                                , (!!)
                                                )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( space )
import           Text.Megaparsec.Char.Lexer     ( decimal
                                                , signed
                                                )

data Moon =
  Moon
    { _x  :: !Int
    , _y  :: !Int
    , _z  :: !Int
    , _dx :: !Int
    , _dy :: !Int
    , _dz :: !Int
    }
  deriving (Eq, Ord, Show)

makeLenses ''Moon

addTriple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTriple (a, b, c) (aa, bb, cc) = (a + aa, b + bb, c + cc)

doTheThing :: [Moon] -> [Moon]
doTheThing moons = do
  self <- moons
  let (gx, gy, gz) = foldl' addTriple (0, 0, 0) $ do
        other <- moons
        guard $ self /= other
        let cmp a b = fromEnum (compare b a) - 1
        pure
          ( cmp (self ^. x) (other ^. x)
          , cmp (self ^. y) (other ^. y)
          , cmp (self ^. z) (other ^. z)
          )
  let applyG = dx %~ (+ gx) >>> dy %~ (+ gy) >>> dz %~ (+ gz)
  let self'  = applyG self
  let add d c = c %~ (+ self' ^. d)
  let self'' = self' & add dx x & add dy y & add dz z
  return self''

readEnergy :: Moon -> Int
readEnergy = (*) <$> readKinetic <*> readPotential
 where
  readKinetic moon = abs (moon ^. dx) + abs (moon ^. dy) + abs (moon ^. dz)
  readPotential moon = abs (moon ^. x) + abs (moon ^. y) + abs (moon ^. z)

parseMoons :: Parser [Moon]
parseMoons = flip sepEndBy space $ do
  let int = signed (pure ()) decimal
  "<x="
  xx <- int
  ", y="
  yy <- int
  ", z="
  zz <- int
  ">"
  return $ Moon xx yy zz 0 0 0

-- |
-- >>> readFile "input/day12" >>= main
-- 5517
-- 303070460651184
main :: Text -> IO ()
main indata = do
  moons <- executeParser parseMoons indata
  moons & iterate doTheThing & (!! 1000) <&> readEnergy & sum & print
  let search key =
        moons
          &   iterate doTheThing
          &   drop 1
          <&> key
          &   elemIndex (key moons)
          <&> (+ 1)
  print $ foldr lcm 1 $ catMaybes
    [ search (<&> view x &&& view dx)
    , search (<&> view y &&& view dy)
    , search (<&> view z &&& view dz)
    ]
