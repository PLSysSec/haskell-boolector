import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)
import Control.Concurrent

main :: IO ()
main = do
  -- Create new Boolector state with a 1s timeout
  bs <- B.newBoolectorState (Just 1)
  B.evalBoolector bs $ do
    -- Create a 8-bit bit-vector
    u8 <- B.bitvecSort 8

    -- Create a constant value and two variables of sort u8
    c <- B.unsignedInt 35 u8
    x <- B.var u8 "x"

    -- Get model
    mc <- B.signedBvConst c
    mx <- B.signedBvConst x
    assert (mc == Just 35) $ return ()
    assert (mx == Nothing) $ return ()

