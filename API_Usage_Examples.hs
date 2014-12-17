import qualified Boolector as B

import Control.Applicative

main = do
    result <- B.withBoolector $ do
        c <- B.unsignedInt 35 8
        x <- B.var 8 ; y <- B.var 8
        p <- B.mul x y ; o <- B.umulo x y
        no <- B.not o ; e <- B.eq c p
        B.assert =<< no B.&& e
        one <- B.one 8
        B.assert =<< B.ugt x one
        B.assert =<< B.ugt y one
        B.dumpSmt2 "dump_example.smt2"
        B.withSolution $ do
            (,) <$> B.val x <*> B.val y
    print result
    
  
