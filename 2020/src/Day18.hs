module Day18 where

import           Protolude                  hiding (many)
import           Text.Megaparsec            hiding (Token)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

data Token
  = Num Int
  | Add
  | Mul
  | Parens [Token]
  deriving (Show)

exprP :: Parser Token
exprP = do
  let parensP = space *> "(" *> exprP <* space <* ")"
  let term    = (Num <$> decimal) <|> parensP
  let opP     = (" + " $> Add) <|> (" * " $> Mul)
  let opTerm  = (\a b -> [a, b]) <$> opP <*> term
  firs <- term
  rest <- many opTerm
  pure $ Parens $ firs : mconcat rest

eval :: Token -> Int
eval (Num    n     ) = n
eval (Parens [expr]) = eval expr
eval (Parens (a : Mul : b : rest)) =
  eval (Parens (Num (eval a * eval b) : rest))
eval (Parens (a : Add : b : rest)) =
  eval (Parens (Num (eval a + eval b) : rest))
eval uwotm8 = panic (show uwotm8)

eval2 :: Token -> Int
eval2 (Num    n     ) = n
eval2 (Parens [expr]) = eval2 expr
eval2 (Parens (a : Add : b : rest)) =
  eval2 (Parens (Num (eval2 a + eval2 b) : rest))
eval2 (Parens (a : Mul : b : rest)) = eval2 a * eval2 (Parens (b : rest))
eval2 uwotm8                        = panic (show uwotm8)

main :: Text -> IO ()
main input = do
  exprs <- parse' (many (exprP <* eol) <* eof) input
  print $ eval <$> exprs & sum
  print $ eval2 <$> exprs & sum
