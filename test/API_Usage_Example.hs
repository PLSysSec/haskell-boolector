import qualified Boolector as B

import Control.Monad.IO.Class
import Control.Exception (assert)

main :: IO ()
main = B.evalBoolector $ do
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
