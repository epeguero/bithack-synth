import Data.SBV
import Data.Int
import Compiler

isPow2ExprSource = "(x != 0) && ((x & (x-1)) == 0)"  
isPow2SpecSource = "i < 8 && x == 1 << i" 

isPow2Expr x = return $ parseZolBv isPow2ExprSource ([("x",x)],[]) :: Predicate

isPow2Spec :: BvT -> BvT -> Predicate
isPow2Spec x i = return $ parseZolBv isPow2SpecSource ([("x",x), ("i",i)],[])

isPow2Theorem = 
    forAll ["x", "i"] (\x i ->
        forSome ["j"] (\j -> 
          (\e spec_i spec_j -> 
            spec_i ==> e &&& ((bnot spec_j) ==> (bnot e)))
          <$> isPow2Expr x <*> isPow2Spec x i <*> isPow2Spec x j))

isPow2HoleExprSource = "(x != 0) && ((x & (x-h)) == 0)"  
isPow2HoleExpr x h = return $ parseZolBv isPow2HoleExprSource ([("x",x), ("h", h)],[]) :: Predicate

isPow2HoleTheorem = 
    forSome ["h"] (\h ->
      forAll ["x", "i"] (\x i ->
          forSome ["j"] (\j -> 
            (\e spec_i spec_j -> 
              spec_i ==> e &&& ((bnot spec_j) ==> (bnot e)))
            <$> isPow2HoleExpr x h <*> isPow2Spec x i <*> isPow2Spec x j)))
