{-# LANGUAGE RecursiveDo #-}
module Compiler where

--import Lang
import Data.SBV
import Text.Earley
import Control.Applicative
import Data.Char


data LangType = Bv

-- Default to 8-bit, signed bitvectors
type BvT = SInt8
type Ctx a = [(String, a)]
type G r a = Grammar r (Prod r () Char a)
type P r a = Prod r () Char a

varP :: P r (Ctx a -> a)
varP = lookupInCtx <$> var
     where lookupInCtx x = \ctx ->
            case flip lookup ctx x of 
              Just v -> v
              Nothing -> error $ "Var " ++ x ++ " not found in context"


bvValP :: P r BvT
bvValP = fromInteger <$> number

bvG :: G r (Ctx BvT -> BvT)
bvG = mdo
  bv <- rule $ buildBinFunApp <$> bv <*> op <*> bv 
           <|> string "(" *> bv <* string ")"
           <|> base
  op <- rule $ (-) <$ string "-" 
           <|> (+) <$ string "+"
           <|> (.&.) <$ string "&"
           <|> sShiftLeft <$ string "<<" 
  base <- rule $ varP <|> const <$> bvValP
  return bv
    where 
      buildBinFunApp ::(Ctx BvT -> BvT) ->
                       (BvT -> BvT -> BvT) -> 
                       (Ctx BvT -> BvT) ->
                       (Ctx BvT -> BvT)
      buildBinFunApp bv1 op bv2 = \ctx -> op (bv1 ctx) (bv2 ctx)

parseBv :: String -> Ctx BvT -> BvT
parseBv s = head . fst $ fullParses (parser $ bvG) s


propBvG :: G r (Ctx BvT -> SBool)
propBvG = bvG >>= (\bv -> mdo
  p <- rule $ buildBinPredApp <$> bv <*> op <*> bv
  op <- rule $ (.==) <$ string "=="
            <|>(./=) <$ string "!="
  return p)
    where 
      buildBinPredApp :: (Ctx BvT -> BvT) ->
                         (BvT -> BvT -> SBool) -> 
                         (Ctx BvT -> BvT) ->
                         (Ctx BvT -> SBool)
      buildBinPredApp bv1 op bv2 = \ctx -> op (bv1 ctx) (bv2 ctx)

parsePropBv :: String -> (Ctx BvT -> SBool)
parsePropBv s = head . fst $ fullParses (parser propBvG) s


zolG :: G r (Ctx BvT -> Ctx SBool -> SBool)
zolG = propBvG >>= (\p -> mdo
  zol <- rule $ buildBinZolApp <$> zol <*> op <*> zol <|> base
  op <- rule $ (&&&) <$ string "&&"
           <|> (==>) <$ string "=>"
  base <- rule $ liftProp <$> p <|> liftVar <$> varP
  return zol)
    where 
      liftProp :: (Ctx BvT -> SBool) -> 
                  (Ctx BvT -> Ctx SBool -> SBool)
      liftProp p = \ctxBvT ctxBool -> p ctxBvT

      liftVar :: (Ctx SBool -> SBool) ->
                 (Ctx BvT -> Ctx SBool -> SBool)
      liftVar x = \ctxBvT ctxBool -> x ctxBool
  
      buildBinZolApp ::(Ctx BvT -> Ctx SBool -> SBool) ->
                       (SBool -> SBool -> SBool) -> 
                       (Ctx BvT -> Ctx SBool -> SBool) ->
                       (Ctx BvT -> Ctx SBool -> SBool)
      buildBinZolApp zol1 op zol2 = 
        \ctxBv ctxBool -> 
          op (zol1 ctxBv ctxBool) (zol2 ctxBv ctxBool)

parseZolBv :: String -> (Ctx BvT -> Ctx SBool -> SBool)
parseZolBv s = head . fst $ fullParses (parser zolG) s

{-
folG :: G r (Ctx a -> SBool)
folG = zolG >>= (\zol -> mdo
  fol <- rule $ buildLet
                <$ string "let" <*> var <*> fol
                <* string "in" <*> zol
            <|> buildExists <$ string "exists"
                            <* some var
                            <* fol
            <|> zol
  op <- rule $ (&&&) <$ string "&&"
  return fol)
    where
      buildLet :: String ->
                  (Ctx a -> SBool) ->
                  (Ctx a -> SBool) ->
                  (Ctx a -> SBool)
      buildLet pvar p1 p2 = 
        \ctx -> let ctx' = ctx::(pvar,p1) 
                in (p1 ctx) (p2 ctx)

-}
----- Low Level Parsers -----
ws = many $ satisfy isSpace :: Prod r e Char String
leadingWs p = ws *> p 

string s = leadingWs $ list s :: Prod r e Char String

number :: Prod r e Char Integer
number = leadingWs $ (read :: String -> Integer) <$> (some . satisfy $ isDigit)

var = leadingWs $ some $ satisfy isAlpha :: Prod r e Char String
