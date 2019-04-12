{-# LANGUAGE RecursiveDo #-} 
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment
import Data.Char
import Text.Earley
import Control.Applicative
import Control.Monad.ST
import Data.Functor (($>))

----- Sketch -----
data Sketch = Term BitVecTerm | Prop BitVecProp deriving Show
sketchGrammar :: forall r. Grammar r (Prod r () Char Sketch)
sketchGrammar =  (\bv zolBv -> Term <$> bv <|> Prop <$> zolBv) <$> bvTermGrammar <*> bvPropGrammar 

sketchParser :: String -> ([Sketch], Report () String)
sketchParser = generateParser sketchGrammar
 
----- Bit Vector Proposition -----
data BitVecProp = BvPropApp BitVecTerm BvProp BitVecTerm
  deriving Show
data BvProp = BvEq | BvNe | BvLt | BvGt | BvLe | BvGe
  deriving Show

bvPropGrammar :: forall r. Grammar r (Prod r () Char BitVecProp)
bvPropGrammar = (\bv -> BvPropApp <$> bv <*> bv_prop <*> bv) <$> bvTermGrammar
  where bv_prop =     BvEq <$ string "=_bv" <|> BvNe <$ string "/=_bv" 
                  <|> BvGt <$ string ">" <|> BvGe <$ string ">=" 
                  <|> BvLt <$ string "<" <|> BvLe <$ string "<="

bvPropParser :: String -> ([BitVecProp], Report () String)
bvPropParser = generateParser bvPropGrammar

----- Bit Vector Term -----
data BitVecTerm = Hole | BitVecVal Int | Var String | BvFunApp BitVecTerm BvFun BitVecTerm
  deriving Show
data BvFun = Bwand | Lshift | Rshift | Plus | Minus
  deriving Show

bvTermGrammar :: forall r. Grammar r (Prod r () Char BitVecTerm)
bvTermGrammar = mdo
  bv1 <- rule $ (BvFunApp <$> bv1) <*> bv_op1 <*> bv1 <|> bv2
  bv2 <- rule $ (BvFunApp <$> bv2) <*> bv_op2 <*> bv2 <|> bv3
  bv3 <- rule $ (BvFunApp <$> bv3) <*> bv_op3 <*> bv3 <|> base

  bv_op1 <- rule $ Bwand <$ string "&"
  bv_op2 <- rule $ Lshift <$ string "<<" <|> Rshift <$ string ">>"
  bv_op3 <- rule $ Plus <$ string "+" <|> Minus <$ string "-"

  base <- rule $ Hole <$ string "??" <|> BitVecVal <$> number <|> Var <$> var

  return bv1

bvTermParser :: String -> ([BitVecTerm], Report () String)
bvTermParser = generateParser bvTermGrammar

----- Low Level Parsers -----
ws = many $ satisfy isSpace :: Prod r e Char String
leadingWs p = ws *> p 

string s = leadingWs $ list s :: Prod r e Char String

number :: Prod r e Char Int
number = leadingWs $ (read :: String -> Int) <$> (some . satisfy $ isDigit)

var = leadingWs $ some $ satisfy isAlpha :: Prod r e Char String


----- User / Debugging Interface -----
main :: IO()
main = do
  x:_ <- getArgs
  printParses $ bvTermParser x

printParses :: forall e a. (Show e, Show a) => ([a], Report e String) -> IO ()
printParses = print

generateParser :: forall e a. 
          (forall r. Grammar r (Prod r e Char a)) -> 
          String -> ([a], Report e String)
generateParser g = f
  where f :: String -> ([a], Report e String)
        f = fullParses p

        p :: Parser e String a
        p = parser g
