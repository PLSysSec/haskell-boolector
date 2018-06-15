# Haskell binding for the Boolector SMT Solver

This is a simple library for writing SMT queries against the Boolector SMT
solver.

## Install

1. Install the Boolector library from <https://github.com/Boolector/boolector>.

   On Arch Linux, you can install Boolector (and the Lingeling SAT solver)
   with: `yaourt -S boolector-git`

2. `cabal install boolector`

## Example

This program (`test/API_Usage_Examples.hs`) shows basic API usage.

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
