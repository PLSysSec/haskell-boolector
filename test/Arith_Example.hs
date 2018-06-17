import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)


main :: IO ()
main = do
  bs <- B.newBoolectorState Nothing
  B.evalBoolector bs $ do
    -- Create sorts:
    i32   <- B.bitvecSort 32
    fSort <- B.funSort [i32] i32
    gSort <- B.funSort [i32, i32] i32

    -- Create variables x, y, z, f, g
    x <- B.var i32 "x"
    y <- B.var i32 "y"
    z <- B.var i32 "z"
    f <- B.uf fSort "f"
    g <- B.uf gSort "g"

    -- Create constant:
    two <- B.signedInt 2 i32
    
    -- Create action to print model
    let printModel = do mx <- B.signedBvAssignment x
                        my <- B.signedBvAssignment y
                        mz <- B.signedBvAssignment z
                        liftIO $ putStrLn $ show [mx, my, mz]
  
    -- (assert (>= (* 2 x) (+ y z)))
    do tmp1 <- B.mul two x
       tmp2 <- B.add y z
       B.assert =<< B.sgte tmp1 tmp2

    -- (assert (< (f x) (g x x)))
    do tmp1 <- B.apply [x] f
       tmp2 <- B.apply [x, x] g
       B.assert =<< B.slt tmp1 tmp2

    -- (assert (> (f y) (g x x)))
    do tmp1 <- B.apply [y] f
       tmp2 <- B.apply [x, x] g
       B.assert =<< B.sgt tmp1 tmp2

    -- Check satisfiability:
    B.Sat <- B.sat

    -- Print model:
    printModel

    -- Push context
    B.push 1

    -- Add (false) assertion:
    B.assert =<< B.eq x y

    -- Check satisfiability:
    B.Unsat <- B.sat

    -- Pop context
    B.pop 1

    -- Can check sat again and pirnt model
    B.Sat <- B.sat
    printModel



{- This example is from https://rise4fun.com/Z3/smtc_arith:

; This example illustrates basic arithmetic and uninterpreted functions
(declare-fun x () Int)
(declare-fun y () Int)
(declare-fun z () Int)
(assert (>= (* 2 x) (+ y z)))
(declare-fun f (Int) Int)
(declare-fun g (Int Int) Int)
(assert (< (f x) (g x x)))
(assert (> (f y) (g x x)))
(check-sat)
(get-model)
(push)
(assert (= x y))
(check-sat)
(pop)
(exit)

-}
