import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)

main :: IO ()
main = do
  bs <- B.newBoolectorState Nothing
  B.evalBoolector bs $ do
    u8 <- B.bitvecSort 8
    u32 <- B.bitvecSort 32

    arr8x32 <- B.arraySort u8 u32

    -- Test isBitvecSort:
    assert (B.isBitvecSort u8) $ return ()
    assert (B.isBitvecSort u32) $ return ()
    -- Test isArraySort:
    assert (B.isArraySort arr8x32) $ return ()

    arr <- B.array arr8x32 "a"

    x <- B.var u8 "x"
    y <- B.unsignedInt 35 u32
    B.write arr x y

    z <- B.unsignedInt 23 u8
    B.assert =<< B.eq x z

    y' <- B.read arr x
    w <- B.var u8 "w"

    B.dump B.DumpSMT2 "dump_example.smt2"
    B.sat
    mx  <- B.unsignedBvAssignment x
    my <- B.unsignedBvAssignment y
    my' <- B.unsignedBvAssignment y'
    mz <- B.unsignedBvAssignment z
    assert (mx == 23) $ return ()
    assert (my == 35) $ return ()
    assert (my' == 35) $ return ()
    assert (mz == 23) $ return ()
