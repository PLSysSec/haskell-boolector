import qualified Boolector.Foreign as BF
import qualified Boolector as B

import Control.Applicative

main = do
    higherlevel >>= print

higherlevel = B.withBoolector $ do
    c <- B.unsignedInt 35 8
    x <- B.var 8 ; y <- B.var 8
    p <- B.mul x y ; o <- B.umulo x y
    no <- B.not o ; e <- B.eq c p
    B.assert =<< B.and no e
    one <- B.one 8
    B.assert =<< B.ugt x one
    B.assert =<< B.ugt y one
    B.withSolution $ do
        (,) <$> B.val x <*> B.val y

lowlevel = do
  b <- BF.new
  BF.setOpt b "model_gen" 2
  c <- BF.unsignedInt b 35 8
  x <- BF.var b 8 "x" ;  y <- BF.var b 8 "y"
  p <- BF.mul b x y ;  o <- BF.umulo b x y
  no <- BF.not b o ;  e <- BF.eq b c p
  BF.assert b =<< BF.and b no e
  one <- BF.one b 8
  BF.assert b =<< BF.ugt b x one
  BF.assert b =<< BF.ugt b y one 
  status <- BF.sat b ; print status
  s <- BF.bvAssignment b x ; print s
  t <- BF.bvAssignment b y ; print t

  
