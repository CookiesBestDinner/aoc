module Day14 where

import qualified Data.Map.Strict            as Map
import           Protolude                  hiding (many, mask)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

data Instr
  = Assign Int Int
  | Mask [MDigit]
  deriving (Show)

data Program
  = Program
      { mem  :: Map Int Int
      , mask :: [MDigit]
      }
  deriving (Show)

data Digit = Zero | One deriving (Enum, Show)
data MDigit = MZero | MOne | MFloat deriving (Show)

parseInstr :: Parser [Instr]
parseInstr = many (parseAssign <|> parseMask) <* eof
 where
  digit       = "1" $> MOne <|> "0" $> MZero <|> "X" $> MFloat
  parseMask   = Mask <$ "mask = " <*> replicateM 36 digit <* eol
  parseAssign = Assign <$ "mem[" <*> decimal <* "] = " <*> decimal <* eol

maskToI :: [Digit] -> Int
maskToI = foldl' (\acc d -> acc * 2 + fromEnum d) 0

iToMask :: Int -> [Digit]
iToMask = reverse . take 36 . (<> repeat Zero) . go
 where
  go 0 = [Zero]
  go 1 = [One]
  go n | even n    = Zero : go (n `div` 2)
       | otherwise = One : go (n `div` 2)

decode :: [MDigit] -> Int -> [Int]
decode mask n = map maskToI $ go mask (iToMask n)
 where
  go (a : as) (b : bs) = do
    let rests = go as bs
    out <- case a of
      MOne   -> [One]
      MZero  -> [b]
      MFloat -> [Zero, One]
    (out :) <$> rests
  go [] [] = [[]]
  go _  _  = panic "unbalanced decode inputs"

step1 :: Program -> Instr -> Program
step1 p (Mask m      ) = p { mask = m }
step1 p (Assign loc n) = p { mem = Map.insert loc val (mem p) }
 where
  val = maskToI $ zipWith digitOr (mask p) (iToMask n)
  digitOr MFloat b = b
  digitOr MZero  _ = Zero
  digitOr MOne   _ = One

step2 :: Program -> Instr -> Program
step2 p (Mask m        ) = p { mask = m }
step2 p (Assign loc val) = p { mem = Map.union insertions (mem p) }
  where insertions = Map.fromList $ (, val) <$> decode (mask p) loc

main :: Text -> IO ()
main input = do
  instr <- parse' parseInstr input
  let z = Program mempty mempty
  print $ foldl' step1 z instr & mem & Map.elems & sum
  print $ foldl' step2 z instr & mem & Map.elems & sum
