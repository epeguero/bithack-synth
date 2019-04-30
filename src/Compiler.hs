{-# LANGUAGE RecursiveDo #-}
module Compiler where

--import Lang
import Data.SBV
import Text.Earley
import Control.Applicative
import Data.Char


-- Default to 8-bit, signed bitvectors
type BvT = SInt64

data Type = Bv | Bool deriving Eq
type Ctx a = [(String, a)]

type G r a = Grammar r (Prod r () Char a)
type P r a = Prod r () Char a

varP :: P r (Ctx a -> a)
varP = lookupInCtx <$> var

bvValP :: P r BvT
bvValP = fromInteger <$> number

bvG :: G r (Ctx BvT -> BvT)
bvG = mdo
  bv <- rule $ binApp <$> bv <*> op <*> bv
           <|> string "(" *> bv <* string ")"
           <|> base
  op <- rule $ (-) <$ string "-"
           <|> (+) <$ string "+"
           <|> (.&.) <$ string "&"
           <|> sShiftLeft <$ string "<<" 
  base <- rule $ varP <|> const <$> bvValP
  return bv

propBvG :: G r (Ctx BvT -> SBool)
propBvG = bvG >>= (\bv -> mdo
  p <- rule $ binApp <$> bv <*> op <*> bv
  op <- rule $ (.==) <$ string "=="
            <|>(./=) <$ string "!="
            <|>(.<) <$ string "<"
  return p)

type LogicCtx a = (Ctx a, Ctx SBool)

zolG :: G r (Ctx a -> SBool) -> G r (LogicCtx a -> SBool)
zolG pG = pG >>= (\p -> mdo
  zol <- rule $ binApp <$> zol <*> op <*> zol 
            <|> base
  op <- rule $ (&&&) <$ string "&&"
           <|> (==>) <$ string "=>"
  base <- rule $ liftProp <$> p 
             <|> liftVar <$> varP
             <|> string "(" *> (liftProp <$> p) <* string ")"
  return zol)
    where 
      liftProp :: (Ctx a -> SBool) -> (Ctx a, Ctx SBool) -> SBool
      liftProp p = \(ctx1, ctx2) -> p ctx1

      liftVar :: (Ctx SBool -> SBool) ->
                 (Ctx a, Ctx SBool) -> SBool
      liftVar x = \(ctx1, ctx2) -> x ctx2
  

folG :: (SymWord a) => G r (Ctx (SBV a) -> SBool) -> G r (LogicCtx (SBV a) -> Predicate)
folG pG = (zolG pG) >>= (\zolP -> mdo
  fol <- rule $ buildExists <$ string "exists" 
                            <*> (var <* string ".")
                            <*> fol
            <|>  (return .) <$> zolP 
  return fol)


parseBv :: String -> Ctx BvT -> BvT
parseBv s = head . fst $ fullParses (parser $ bvG) s

parsePropBv :: String -> (Ctx BvT -> SBool)
parsePropBv s = head . fst $ fullParses (parser propBvG) s

parseZolBv :: String -> ((Ctx BvT, Ctx SBool) -> SBool)
parseZolBv s = head . fst $ fullParses (parser $ zolG propBvG) s

parseFolBv :: String -> (LogicCtx BvT -> Predicate)
parseFolBv s = head . fst $ fullParses (parser $ folG propBvG) s


----- Term Building Utilities -----
lookupInCtx :: String -> Ctx a -> a
lookupInCtx x ctx =
    case flip lookup ctx x of
      Just v -> v
      Nothing -> error $ "Error: Var " ++ x ++ " not found in context"

binApp :: (ctx -> a) -> 
          (a -> b -> c) -> 
          (ctx -> b) ->
          (ctx -> c)
binApp t1 op t2 = \ctx -> op (t1 ctx) (t2 ctx)


buildExists :: (SymWord t) => String -> (LogicCtx (SBV t) -> Predicate) -> (LogicCtx (SBV t) -> Predicate)
buildExists x fol = \(ctx1,ctx2) ->
  forSome [x] (\v -> fol ((x,v):ctx1, ctx2))

----- Low Level Parsers -----
ws = many $ satisfy isSpace :: Prod r e Char String
leadingWs p = ws *> p 

string s = leadingWs $ list s :: Prod r e Char String

number :: Prod r e Char Integer
number = leadingWs $ (read :: String -> Integer) <$> (some . satisfy $ isDigit)

var = leadingWs $ some $ satisfy isAlpha :: Prod r e Char String
