# Haskell Binding for SMT Solver Boolector.

<http://fmv.jku.at/boolector/> 

The binding is a (quite) low-level translation
of Boolector's API.

## Example.

This program (`API_Usage_Examples.hs`)
```
import qualified Boolector as B

import Control.Applicative

main = do
    result <- B.withBoolector $ do
        c <- B.unsignedInt 35 8
        x <- B.var 8 ; y <- B.var 8
        p <- B.mul x y ; o <- B.umulo x y
        no <- B.not o ; e <- B.eq c p
        B.assert =<< no B.&& e
        one <- B.one 8
        B.assert =<< B.ugt x one
        B.assert =<< B.ugt y one
        B.dumpSmt2 "dump_example.smt2"
        B.withSolution $ do
            (,) <$> B.val x <*> B.val y
    print result  
```
will print
```
Just (7,5)
```
Note that
* names are as in `boolector.h` (but `boolector_and` is `&&` and `boolector_or` is `||`)
* the program is `do <build formula> ; withSolution <access model>`

## Installing, Licensing

The script  `build.sh`  will download and compile
Boolector sources.
If you run it, you accept Boolector's licensing terms.

The default license does not allow this software
to be used in a commercial context
nor as part of a submission to a competition
or a similar event.

For more details see COPYING respectively LICENSE
in the individual archives in the 'archives' directory.
