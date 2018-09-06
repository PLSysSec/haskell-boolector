import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)


main :: IO ()
main = do
  bs <- B.newBoolectorState Nothing
  B.evalBoolector bs $ do
    -- Create sort:
    u64   <- B.bitvecSort 64

    -- Test isBitvecSort:
    assert (B.isBitvecSort u64) $ return ()

    -- Create variables x and y
    x <- B.var u64 "x"
    y <- B.var u64 "y"

    -- Create constants:
    big   <- B.unsignedInt _BIG u64
    small <- B.unsignedInt _SMALL u64
    
    -- Create action to print model
    let printModel = do mx <- B.unsignedBvAssignment x
                        my <- B.unsignedBvAssignment y
                        assert (mx == _BIG) $ return ()
                        assert (my == _BIG) $ return ()
                        liftIO $ putStrLn $ show [mx, my]
  
    -- (assert (= x y))
    B.assert =<< B.eq x y
    -- (assert (= x big))
    B.assert =<< B.eq x big
    -- (assert (= small big))
    B.assert =<< B.ne small big

    -- Print SMT2 file
    smt <- B.dumpToString B.DumpSMT2
    liftIO $ putStrLn smt

    -- Check satisfiability:
    B.Sat <- B.sat

    -- Print model:
    printModel

  where _BIG   = 18446744073709551615
        _SMALL = 4294967295 
