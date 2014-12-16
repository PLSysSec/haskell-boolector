Haskell Binding for SMT Solver Boolector.

<http://fmv.jku.at/boolector/> 

The binding is a low-level translation of Boolector's API.

Example. This program (`API_Usage_Example.hs`)
```
import qualified Boolector as B

main = do
  b <- B.new
  B.setOpt b "model_gen" 2
  c <- B.unsignedInt b 35 8
  x <- B.var b 8 "x" ;  y <- B.var b 8 "y"
  p <- B.mul b x y ;  o <- B.umulo b x y
  no <- B.not b o ;  e <- B.eq b c p
  B.assert b =<< B.and b no e
  one <- B.one b 8
  B.assert b =<< B.ugt b x one
  B.assert b =<< B.ugt b y one 
  status <- B.sat b ; print status
  s <- B.bvAssignment b x ; print s
  t <- B.bvAssignment b y ; print t

```
will print
```
SAT
"00000111"
"00000101"
```

The script  build.sh  will download and compile
Boolector sources.
If you run it, you accept Boolector's licensing terms.

The default license does not allow this software
to be used in a commercial context
nor as part of a submission to a competition
or a similar event.

For more details see COPYING respectively LICENSE
in the individual archives in the 'archives' directory.
