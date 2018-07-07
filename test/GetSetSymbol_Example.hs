
import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)
import Control.Concurrent

-- This example is same as the API_Usage_Example, but additionally tests
-- symbols getters and setters

main :: IO ()
main = do
  -- Create new Boolector state with a 1000ms timeout
  bs <- B.newBoolectorState (Just 1000)
  B.evalBoolector bs $ do
    -- Create a 8-bit bit-vector
    u8 <- B.bitvecSort 8

    -- Create a constant value and two variables of sort u8
    c <- B.unsignedInt 35 u8
    x <- B.var u8 "x"
    y <- B.var u8 "y"

    B.getSymbol x >>= \xname -> assert (xname == Just "x") $ return ()
    B.getSymbol y >>= \yname -> assert (yname == Just "y") $ return ()

    -- Perform some operations on the values
    p  <- B.mul x y
    o  <- B.umulo x y
    no <- B.not o
    e  <- B.eq c p

    B.getSymbol p >>= \pname -> assert (pname == Nothing) $ return ()
    B.setSymbol p "p"
    B.getSymbol p >>= \pname -> assert (pname == Just "p") $ return ()


    -- Make some assertions
    B.assert =<< B.and no e
    one <- B.one u8
    B.assert =<< B.ugt x one
    B.assert =<< B.ugt y one

    -- Dump the corresponding SMT Lib 2
    B.dumpToString B.DumpSMT2 >>= liftIO . putStrLn

    -- Check satisfiability
    B.Sat <- B.sat

    -- Get model
    mx <- B.unsignedBvAssignment x
    my <- B.unsignedBvAssignment y
    mp <- B.unsignedBvAssignment p
    assert (mx == 7) $ return ()
    assert (my == 5) $ return ()
    assert (mp == 35) $ return ()
