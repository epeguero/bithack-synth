{-# LANGUAGE RecursiveDo #-} 
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment
import Data.Char
import Text.Earley
import Control.Applicative
import Data.Foldable (asum)

type Gram r e ast = Grammar r (Prod r e Char ast)
type ParserOutput e ast = ([ast], Report e String)

----- Supported Theories -----
data Term t = VarT String | ValT (Value t) | BinT (Term t) (FunT t) (Term t)
data Prop t = BinP (Term t) (FunP t) (Term t)

class SupportedTheory t where
  data Value t :: *
  data FunT t :: *
  data FunP t :: *

  value :: Prod r e Char (Value t)
  funTPrec :: [[FunT t]]
  funPPrec :: [[FunP t]]
  funTtoString :: FunT t -> String
  funPtoString :: FunP t -> String

  term :: Gram r e (Term t)
  term = foldr ((=<<) . termPrec) base funTPrec
    where 
      termPrec :: [FunT t] -> Prod r e Char (Term t) -> Gram r e (Term t)
      termPrec ops next = mdo  
        t <- rule $ BinT <$> t <*> op <*> t <|> next
        op <- rule $ 
              asum --foldr (<|>) empty 
              . map (\op -> op <$ (string . funTtoString $ op))
              $ ops
        return t

      base :: Gram r e (Term t)
      base = rule $ ValT <$> value <|> VarT <$> var

  termParser :: String -> ParserOutput e (Term t)
  termParser = fullParses p
    where
      p :: Parser e String (Term t)
      p = parser term


  prop :: Gram r e (Prop t)
  prop = foldr ((=<<) . propPrec) (rule empty) funPPrec
    where 
      propPrec :: [FunP t] -> Prod r e Char (Prop t) -> Gram r e (Prop t)
      propPrec ops next = (\t -> mdo  
        p <- rule $ BinP <$> t <*> op <*> t <|> next
        op <- rule $ 
              asum --foldr (<|>) empty 
              . map (\op -> op <$ (string . funPtoString $ op))
              $ ops
        return p) =<< term

  propParser :: String -> ParserOutput e (Prop t)
  propParser = fullParses p
    where
      p :: Parser e String (Prop t)
      p = parser prop


data BitVectorTheory
instance SupportedTheory BitVectorTheory where
  data Value BitVectorTheory = HoleBv | BitVecVal Int
    deriving Show
  data FunT BitVectorTheory = Bwand | Lshift | Rshift | Plus | Minus
    deriving Show
  data FunP BitVectorTheory = BvEq | BvNe | BvLt | BvGt | BvLe | BvGe
    deriving Show

  value = HoleBv <$ string "??" <|> BitVecVal <$> number

  funTPrec = [[Bwand], [Lshift, Rshift], [Plus, Minus]]
  funPPrec = [[BvEq, BvNe], [BvGt, BvLt, BvGe, BvLe]]

  funTtoString Bwand = "&"
  funTtoString Lshift = "<<"
  funTtoString Rshift = ">>"
  funTtoString Plus = "+"
  funTtoString Minus = "-"

  funPtoString BvEq = "=_bv"
  funPtoString BvNe = "/=_bv"
  funPtoString BvLt = "<"
  funPtoString BvGt = ">"
  funPtoString BvLe = "<="
  funPtoString BvGe = ">="


instance Show (Term BitVectorTheory) where
  show (VarT x) = show x
  show (ValT v) = show v
  show (BinT t1 op t2) = "( " ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ " )"

instance Show (Prop BitVectorTheory) where
  show (BinP t1 op t2) = "( " ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ " )"

{-

----- Sketch -----
data Sketch = TermBv BitVecTerm | PropBv BitVecProp deriving Show
sketchGrammar :: Gram Sketch
sketchGrammar =  (\bv zolBv -> TermBv <$> bv <|> PropBv <$> zolBv) 
                  <$> bvTermGrammar <*> bvPropGrammar 

sketchParser :: String -> ParserOutput () Sketch
sketchParser = generateParser sketchGrammar
 

----- Bit Vector Proposition -----
data BitVecProp = BvPropApp BitVecTerm BvProp BitVecTerm
  deriving Show
data BvProp = BvEq | BvNe | BvLt | BvGt | BvLe | BvGe
  deriving Show

bvPropGrammar :: Gram BitVecProp
bvPropGrammar = (\bv -> BvPropApp <$> bv <*> bv_prop <*> bv) <$> bvTermGrammar
  where bv_prop =     BvEq <$ string "=_bv" <|> BvNe <$ string "/=_bv" 
                  <|> BvGt <$ string ">" <|> BvGe <$ string ">=" 
                  <|> BvLt <$ string "<" <|> BvLe <$ string "<="

bvPropParser :: String -> ParserOutput () BitVecProp
bvPropParser = generateParser bvPropGrammar


----- Bit Vector Term -----
data BitVecTerm = Hole | BitVecVal Int | Var String | BvFunApp BitVecTerm BvFun BitVecTerm
  deriving Show
data BvFun = Bwand | Lshift | Rshift | Plus | Minus
  deriving Show

bvTermGrammar :: Gram BitVecTerm
bvTermGrammar = mdo
  bv1 <- rule $ (BvFunApp <$> bv1) <*> bv_op1 <*> bv1 <|> bv2

  bv_op1 <- rule $ Bwand <$ string "&"
  bv_op2 <- rule $ Lshift <$ string "<<" <|> Rshift <$ string ">>"
  bv_op3 <- rule $ Plus <$ string "+" <|> Minus <$ string "-"

  base <- rule $ Hole <$ string "??" <|> BitVecVal <$> number <|> Var <$> var

  return bv1

bvTermParser :: String -> ParserOutput () BitVecTerm
bvTermParser = generateParser bvTermGrammar


----- Zero-order logic -----
-- data ZOL t = BoolZol Bool | Not (Zol t) | BinPropZol (ZOL t) BinOpZol (Zol t) | TheoryProp t
-- data BinOpZol = AndZol | OrZol | ImpliesZol


-}
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
  bvTerm x 

bvTerm :: String -> IO ()
bvTerm s = print (termParser s :: ParserOutput () (Term BitVectorTheory))

bvProp:: String -> IO ()
bvProp s = print (propParser s :: ParserOutput () (Prop BitVectorTheory))
