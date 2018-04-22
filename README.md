# Haskell Binding for SMT Solver Boolector.

<http://fmv.jku.at/boolector/> 

The binding is a (quite) low-level translation of Boolector's API.

## Example

This program (`API_Usage_Examples.hs`) shows basic API usage.

```haskell
import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)

main :: IO ()
main = do
  bs <- B.newBoolectorState (Just 1000)
  B.evalBoolector bs $ do
    u8 <- B.bitvecSort 8
    
    c <- B.unsignedInt 35 u8
    x <- B.var u8 "x"
    y <- B.var u8 "y"

    p  <- B.mul x y
    o  <- B.umulo x y
    no <- B.not o
    e  <- B.eq c p

    B.assert =<< B.and no e
    one <- B.one u8
    B.assert =<< B.ugt x one
    B.assert =<< B.ugt y one
    B.dumpSmt2 "dump_example.smt2"
    B.sat
    mx <- B.unsignedBvAssignment x
    my <- B.unsignedBvAssignment y
    assert (mx == 7) $ return ()
    assert (my == 5) $ return ()
```

## Installing, Licensing

You can install `libboolector` globally by going into the `boolector-*`
directory and running `make install`.

The current license is poisonous and does not allow this software to be used in
a commercial context nor as part of a submission to a competition or a similar
event. This project will remain private until that changes.
