{-# LANGUAGE RecursiveDo #-} 
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, UndecidableInstances #-}

module Main where

import System.Environment
import Data.Char
import Text.Earley
import Control.Applicative
import Data.Foldable (asum)

type Gram r e ast = Grammar r (Prod r e Char ast)
type ParserOutput e ast = ([ast], Report e String)

main = print "Hey"

type family Value t
type family FunF t 
type family FunP t

data Formula t = Val (Value t) | AppF (FunF t) [Formula t]
deriving instance (Show (Value t), Show (FunF t)) => Show (Formula t)
data Prop t = AppP (FunP t) [Formula t]
deriving instance (Show (Value t), Show (FunP t), Show (FunF t)) => Show (Prop t)

-- Defining sketches orthogonally from theory
data ValSketch :: * -> *
type instance Value (ValSketch t) = Maybe (Value t)
type instance FunF (ValSketch t) = FunF t
type instance FunP (ValSketch t) = FunP t

type instance Value (Either t1 t2) = Either (Value t1) (Value t2)
type instance FunF (Either t1 t2) = Either (Value t1) (Value t2)

type Expr = Formula (ValSketch BitVectorTheory) -- (Logic FirstOrder (Theory BitVector))


-- Bit Vector Theory instantiation
data BitVectorTheory 
type instance Value BitVectorTheory = Int
type instance FunF BitVectorTheory = BitVectorFunctions
type instance FunP BitVectorTheory = BitVectorPropositions
data BitVectorFunctions = Lshift | Rshift
  deriving Show
data BitVectorPropositions = BvEq | BvLt
  deriving Show






{-
----- Supported Theories -----
data Term t = VarT String | ValT (Value t) | AppT (FunT t) [(Term t)]
data Prop t = AppP (FunP t) [(Term t)]

class (Show (FunT t), Show (FunP t)) => SupportedTheory t where
  data Value t :: *
  data FunT t :: *
  data FunP t :: *

  value :: Prod r e Char (Value t)
  funTPrec :: [[FunT t]]
  funPPrec :: [[FunP t]]

  term :: Gram r e (Term t)
  term = foldr ((=<<) . termPrec) base funTPrec
    where 
      termPrec :: [FunT t] -> Prod r e Char (Term t) -> Gram r e (Term t)
      termPrec ops next = mdo  
        t <- rule $ buildAppT <$> t <*> op <*> t <|> next
        op <- rule $ 
              asum --foldr (<|>) empty 
              . map (\op -> op <$ (string . show $ op))
              $ ops
        return t

      base :: Gram r e (Term t)
      base = rule $ ValT <$> value <|> VarT <$> var
 
      buildAppT :: Term t -> FunT t -> Term t -> Term t
      buildAppT t1 op t2 = AppT op [t1, t2]

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
        p <- rule $ buildProp <$> t <*> op <*> t <|> next
        op <- rule $ 
              asum 
              . map (\op -> op <$ (string . show $ op))
              $ ops
        return p) =<< term
  
      buildProp :: Term t -> FunP t -> Term t -> Prop t
      buildProp t1 op t2 = AppP op [t1, t2]

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
  data FunP BitVectorTheory = BvEq | BvNe | BvLt | BvGt | BvLe | BvGe

  value = HoleBv <$ string "??" <|> BitVecVal <$> number

  funTPrec = [[Bwand], [Lshift, Rshift], [Plus, Minus]]
  funPPrec = [[BvEq, BvNe], [BvGt, BvLt, BvGe, BvLe]]


instance Show (FunT BitVectorTheory) where
  show Bwand = "&"
  show Lshift = "<<"
  show Rshift = ">>"
  show Plus = "+"
  show Minus = "-"

instance Show (FunP BitVectorTheory) where
  show BvEq = "==_bv"
  show BvNe = "/=_bv"
  show BvLt = "<"
  show BvGt = ">"
  show BvLe = "<="
  show BvGe = ">="

instance Show (Term BitVectorTheory) where
  show (VarT x) = show x
  show (ValT v) = show v
  show (AppT op [t]) = "( " ++ show op ++ show t ++ " )"
  show (AppT op [t1, t2]) = "( " ++ show t1 ++ " " ++ show op 
                                 ++ " " ++ show t2 ++ " )"

instance Show (Prop BitVectorTheory) where
  show (AppP op [t1, t2]) = "( " ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ " )"


data Logic [t] LOrder
data LOrder = Zero | First
data Logic [t] Zero = Boolean0 Bool | MT0 (Prop [t]) | AppL0 FunL [Logic [t] Zero]
  deriving Show
data Logic [t] First = Boolean1 Bool | MT1 (Prop [t]) | AppL1 FunL [Logic [t] Zero]
                       Forall (Logic [t] First) | Exists (Logic [t] First) | 
                       Let String (Logic [t] First) (Logic [t] First)
  deriving Show
-}
----- Low Level Parsers -----
ws = many $ satisfy isSpace :: Prod r e Char String
leadingWs p = ws *> p 

string s = leadingWs $ list s :: Prod r e Char String

number :: Prod r e Char Int
number = leadingWs $ (read :: String -> Int) <$> (some . satisfy $ isDigit)

var = leadingWs $ some $ satisfy isAlpha :: Prod r e Char String


{-
----- User / Debugging Interface -----
main :: IO()
main = do
  x:_ <- getArgs
  bvTerm x 

bvTerm :: String -> IO ()
bvTerm s = print (termParser s :: ParserOutput () (Term BitVectorTheory))

bvProp:: String -> IO ()
bvProp s = print (propParser s :: ParserOutput () (Prop BitVectorTheory))
-}
