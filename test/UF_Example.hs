import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)


main :: IO ()
main = do
  bs <- B.newBoolectorState Nothing
  B.evalBoolector bs $ do
    -- Create sorts:
    u32 <- B.bitvecSort 32
    fSort <- B.funSort [u32] u32

    -- Create variables f, a, and b:
    f <- B.uf fSort "f"
    a <- B.var u32 "a"
    b <- B.var u32 "b"

    c20 <- B.unsignedInt 20 u32
    c10 <- B.unsignedInt 10 u32
    c1  <- B.one u32

    -- Make assertions:
    B.assert =<< B.ugt a c20
    B.assert =<< B.ugt b a

    res <- B.apply [c10] f
    B.assert =<< B.eq res c1

    -- Check satisfiability:
    B.Sat <- B.sat

    -- Get model:
    ma  <- B.unsignedBvAssignment a
    mb  <- B.unsignedBvAssignment b

    -- Check model:
    assert (ma == 21) $ return ()
    assert (mb == 22) $ return ()

{- This example is from https://rise4fun.com/z3/tutorialcontent/guide#h23
  (declare-fun f (Int) Int)
  (declare-fun a () Int) ; a is a constant
  (declare-const b Int) ; syntax sugar for (declare-fun b () Int)
  (assert (> a 20))
  (assert (> b a))
  (assert (= (f 10) 1))
  (check-sat)
  (get-model)
-}
