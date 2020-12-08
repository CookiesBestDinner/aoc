module Day08 where

import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Parsing

data Ins
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

data Comp
  = Comp
      { acc :: Int
      , pc  :: Int
      }
  deriving (Show)

pIns :: Parser Ins
pIns = ("nop " $> Nop <|> "acc " $> Acc <|> "jmp " $> Jmp) <*> number

run :: Map Int Ins -> Comp -> Either Comp Comp
run ins = go mempty
 where
  go seen comp
    | pc comp `M.notMember` ins = Left comp
    | pc comp `S.member` seen = Right comp
    | otherwise = case ins M.! pc comp of
      Nop _ -> go newseen comp { pc = pc comp + 1 }
      Acc n -> go newseen comp { pc = pc comp + 1, acc = acc comp + n }
      Jmp n -> go newseen comp { pc = pc comp + n }
    where newseen = S.insert (pc comp) seen

swapIns :: Map Int Ins -> Int -> Either (Map Int Ins) (Map Int Ins)
swapIns ins i = case ins M.! i of
  Jmp n -> Right $ M.insert i (Nop n) ins
  Nop n -> Right $ M.insert i (Jmp n) ins
  Acc _ -> Left ins

main :: Text -> IO ()
main input = do
  ilist <- parse' (pIns `sepEndBy` space <* eof) input
  let imap = M.fromList $ zip [0 ..] ilist
  print $ run imap (Comp 0 0)
  let candidates = rights $ swapIns <$> [imap] <*> [0 .. length ilist - 1]
  forM_ candidates $ \alt -> case run alt (Comp 0 0) of
    Right _ -> pure ()
    Left  c -> print c
