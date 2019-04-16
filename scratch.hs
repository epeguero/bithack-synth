{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

import Text.Earley
import Data.Char
import Control.Applicative


-- Generic Theory Formula
data TheoryOf t = Theory t
data family Value t 
data family Fun t 

data family Formula t
data instance Formula (TheoryOf t) =
    Val (Value (TheoryOf t)) 
  | App (Fun (TheoryOf t)) [Formula (TheoryOf t)]
deriving instance (Show (Value (TheoryOf t)), Show (Fun (TheoryOf t))) => Show (Formula (TheoryOf t))


-- Generic Sketch
data SketchOf f t = Sketch (f t)
data instance Formula (SketchOf TheoryOf t) =
    SkVal (Maybe (Value (TheoryOf t)))
  | SkApp (Fun (TheoryOf t)) [Formula (SketchOf TheoryOf t)]
deriving instance (Show (Value (TheoryOf t)), Show (Fun (TheoryOf t))) => Show (Formula (SketchOf TheoryOf t))


-- Logic
data family Logic o t
data Zeroth
data First

data Prop t = Prop (Pred t) [Formula t]
data family Pred t
data PredFrom f t = Pred (f t)
data instance Value (Logic Zeroth t) = LTrue | LFalse | Pmt (Prop t)
data instance Fun (Logic Zeroth t) = And | Or | Not | Implies

data instance Formula (Logic First t) =
    Forall String (Formula (Logic First t))
  | Exists String (Formula (Logic First t))
  | Let String (Formula (Logic First t)) (Formula (Logic First t))
  | Zol (Formula (Logic Zeroth t))


-- Putting it all together... Instantiating theories
-- Theory of Bitvectors 
data BitVectors

data instance Value (TheoryOf BitVectors) = BvVal Int
data instance Fun (TheoryOf BitVectors) = BvAdd
data instance Pred (TheoryOf BitVectors) = BvEq

deriving instance Show (Value (TheoryOf BitVectors))
deriving instance Show (Fun (TheoryOf BitVectors))
deriving instance Show (Pred (TheoryOf BitVectors))

-- Programming Language Definition
data LangProps = LangPbv (Prop (TheoryOf BitVectors))
data SpecProps = SpecPbv (Prop (TheoryOf BitVectors))

deriving instance (Show (Prop (TheoryOf BitVectors))) => Show LangProps
deriving instance (Show (Prop (TheoryOf BitVectors))) => Show SpecProps

data LangSketch = 
  LangF (Formula (SketchOf TheoryOf BitVectors)) |
  LangP (Formula (SketchOf (Logic Zeroth) (TheoryOf BitVectors)))
  

-- Example functions over the defined types
bvValParser :: Parser () String (Value (TheoryOf BitVectors))
bvValParser = parser (rule $ BvVal <$> number)
   
parseBvVal :: String -> Value (TheoryOf BitVectors)
parseBvVal s = head . fst $ fullParses bvValParser s


bvFormulaSketchParser :: Parser () String (Formula (SketchOf TheoryOf BitVectors)) 
bvFormulaSketchParser = parser (mdo 
  f <- rule $ v <|> (buildSkApp <$> f <*> string "+" <*> f)
  v <- rule $ SkVal . Just . BvVal <$> number
  return f)
    where buildSkApp f1 s f2 = SkApp BvAdd [f1, f2]

parseBvFormulaSketch :: String -> Formula (SketchOf TheoryOf BitVectors)
parseBvFormulaSketch s = head . fst $ fullParses bvFormulaSketchParser s

----- Low Level Parsers -----
ws = many $ satisfy isSpace :: Prod r e Char String
leadingWs p = ws *> p 

string s = leadingWs $ list s :: Prod r e Char String

number :: Prod r e Char Int
number = leadingWs $ (read :: String -> Int) <$> (some . satisfy $ isDigit)

var = leadingWs $ some $ satisfy isAlpha :: Prod r e Char String
