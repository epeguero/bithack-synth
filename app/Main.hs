{-# LANGUAGE RecursiveDo #-} 

module Main where

import System.Environment
import Data.Char
import Text.Earley
import Control.Applicative
import Data.Functor (($>))

main :: IO()
main = do
  x:_ <- getArgs
  print $ parse x 

parse = fullParses (parser bv)

data BitVecTerm = Hole | BitVecVal Int | Var String | App BitVecTerm Fun BitVecTerm
  deriving Show
data Fun = Bwand | Lshift | Rshift | Plus | Minus
  deriving Show

bv :: Grammar r (Prod r () Char BitVecTerm)
bv = mdo
  bv1 <- rule $ (App <$> bv1) <*> bv_op1 <*> bv1 <|> bv2
  bv2 <- rule $ (App <$> bv2) <*> bv_op2 <*> bv2 <|> bv3
  bv3 <- rule $ (App <$> bv3) <*> bv_op3 <*> bv3 <|> base

  bv_op1 <- rule $ Bwand <$ token "&"
  bv_op2 <- rule $ Lshift <$ token "<<" <|> Rshift <$ token ">>"
  bv_op3 <- rule $ Plus <$ token "+" <|> Minus <$ token "-"

  base <- rule $ Hole <$ token "??" <|> BitVecVal <$> number <|> Var <$> var

  return bv1
  where
    ws = many $ satisfy isSpace :: Prod r e Char String
    leading_ws p = ws *> p 

    token s = leading_ws $ list s :: Prod r e Char String

    number :: Prod r e Char Int
    number = leading_ws $ (read :: String -> Int) <$> (some . satisfy $ isDigit)

    var = leading_ws $ some $ satisfy isAlpha :: Prod r e Char String
