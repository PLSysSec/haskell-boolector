[![Hackage](https://img.shields.io/hackage/v/boolector.svg)](https://hackage.haskell.org/package/boolector)

# Haskell binding for the Boolector SMT Solver

This is a simple library for writing SMT queries against the Boolector SMT
solver.

## Installing Boolector and this library

1. Install the Boolector library from <https://github.com/Boolector/boolector>.

   On Arch Linux, you can install Boolector (and the Lingeling SAT solver)
   with: `yaourt -S boolector-git`

2. `cabal install boolector`

## Example

This program (`test/API_Usage_Examples.hs`) shows basic API usage:

```haskell
import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)
import Control.Concurrent

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

    -- Perofmr some operations on the values
    p  <- B.mul x y
    o  <- B.umulo x y
    no <- B.not o
    e  <- B.eq c p

    -- Make some assertions
    B.assert =<< B.and no e
    one <- B.one u8
    B.assert =<< B.ugt x one
    B.assert =<< B.ugt y one

    -- Dump the corresponding SMT Lib 2 to a file
    B.dump B.DumpSMT2 "dump_example.smt2"

    -- Check satisfiability
    B.Sat <- B.sat

    -- Get model
    mx <- B.unsignedBvAssignment x
    my <- B.unsignedBvAssignment y
    assert (mx == 7) $ return ()
    assert (my == 5) $ return ()
```
