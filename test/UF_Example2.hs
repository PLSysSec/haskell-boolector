import qualified Boolector as B
import Control.Monad.IO.Class

main :: IO ()
main = do
  bs <- B.newBoolectorState Nothing
  B.evalBoolector bs $ do
    -- Create sorts:
    i32   <- B.bitvecSort 32
    fSort <- B.funSort [i32] i32
    
    -- Create constants:
    one   <- B.signedInt 1 i32
    two   <- B.signedInt 2 i32
    five  <- B.signedInt 5 i32
    fifty <- B.signedInt 50 i32

    -- Create variable and function:
    x <- B.var i32 "x"
    f <- B.uf fSort "f"

    -- f(x) >= 5 && f(x) < 50
    do tmp1 <- B.apply [x] f 
       B.assert =<< B.sgte tmp1 five
       B.assert =<< B.slt tmp1 fifty

    -- f(2) + 1 > 5 && f(2) <= 50
    do tmp1 <- B.apply [two] f
       tmp2 <- B.add tmp1 one
       B.assert =<< B.sgte tmp2 five
       B.assert =<< B.slte tmp2 fifty

    -- x >= 2
    B.assert =<< B.sgte x two

    -- Check satisfiability:
    B.Sat <- B.sat

    return ()

