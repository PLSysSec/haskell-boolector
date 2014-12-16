import qualified Boolector as B

import Control.Applicative

main = do
  b <- B.new
  B.setOpt b "model_gen" 2
  one <- B.one b 8
  c <- B.unsignedInt b 35 8
  x <- B.var b 8 "x"
  y <- B.var b 8 "y"
  p <- B.mul b x y
  o <- B.umulo b x y
  no <- B.not b o
  e <- B.eq b c p
  B.assert b =<< B.and b no e
  B.assert b =<< B.ugt b x one
  B.assert b =<< B.ugt b y one 
  status <- B.sat b
  print status
  s <- B.bvAssignment b x ; print s
  t <- B.bvAssignment b y ; print t
  
