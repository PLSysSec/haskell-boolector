{-|


This module presents Boolector functions via a monadic API.

Typical usage:

@
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
@

This will print @Just (7,5)@

Note:

* the program is 'do <build formula> ; withSolution <access model>'

* For semantics of operations, see <http://fmv.jku.at/boolector/doc/pyboolector.html>

* names are as given there, but 'boolector_and' is @&&@ and 'boolector_or' is @||@

-}

{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
    
module Boolector

( module Boolector 
, Node ()
)       

where

import qualified Boolector.Foreign as B
import Boolector.Foreign (Node)

import Control.Monad.State.Strict
import Control.Applicative
import Data.Char (isDigit)

import Control.Exception (bracket, finally, mask_, onException )
import Control.Concurrent.Async
import System.IO

import Prelude hiding (read, not, and, or, (&&), (||))
import qualified Prelude

-- * The monad

data S =
     S { btor :: ! B.Tor -- ^  solver instance
       , next :: ! Int -- ^ for counting names
       , solved :: ! Bool
       }  

newtype Boolector a = Boolector { un :: StateT S IO a }
    deriving (Functor, Applicative, Monad, MonadState S, MonadIO )

-- | build a formula, solve it, and process the solution.
withBoolector :: (Boolector (Boolector a))
         -> IO (Maybe a)
withBoolector action = do
    b <- B.new
    withBtor b action

withBtor b action = do    
    B.setOpt b "model_gen" 2
    B.setOpt b "auto_cleanup" 1
    let s0 = S { btor = b , next = 0, solved = False }
    cont <- evalStateT (un action) s0
    status <- B.sat b
    case status of
        B.SAT -> do
            Just <$> evalStateT (un cont) s0 { solved = True }
        _ -> return Nothing

withBoolectorAsync :: (Boolector (Boolector a))
         -> IO (Maybe a)
withBoolectorAsync action = bracket B.new B.delete $ \ b -> do
  mask_ $ withAsync (withBtor b action) $ \ a -> do
    wait a `onException` do
      hPutStrLn stderr "*** want to interrupt Boolector here ***"

-- | run the action after a solution was found.
withSolution action = return action

-- | create a fresh integer unknown. argument: bit width
var :: Int -> Boolector B.Node
var width = do
    s <- get
    put $ s { next = succ $ next s }
    wrap2 B.var width $ "var" ++ (show $ next s)

-- | get the value for this variable.    
val :: B.Node -> Boolector Integer
val n = do
    s <- get
    when (Prelude.not $ solved s) $ error "Boolector.value: may only occur after withSolution"
    a <- wrap1 B.bvAssignment n
    when (Prelude.not $ all isDigit a) $ error $ "Boolector.value: not numeric: " ++ a
    return $ foldl ( \ n c -> 2 * n + Prelude.read [c] ) 0 a

getWidth = wrap1 B.getWidth

signedVal :: B.Node -> Boolector Integer
signedVal n = do
    w <- getWidth n
    v <- val n
    let h = 2 ^ pred w
    return $ if v >= h then v - (2*h) else v

-- | get Boolean value (one bit)
bval n = do
    v <- val n
    return $ case v of
        0 -> False
        1 -> True
        _ -> error $ "Boolector.bval: not boolean: " ++ show v

wrap0 action = do
    s <- get ; liftIO $ action ( btor s )
wrap1 action x1 = do
    s <- get ; liftIO $ action ( btor s ) x1
wrap2 action x1 x2 = do
    s <- get ; liftIO $ action ( btor s ) x1 x2
wrap3 action x1 x2 x3 = do
    s <- get ; liftIO $ action ( btor s ) x1 x2 x3

-- * Operations

and [] = true ; and (x:xs) = foldM (&&) x xs
or  [] = false ; or (x:xs) = foldM (||) x xs

assert = wrap1 B.assert
false = wrap0 B.false
zero = wrap1 B.zero
true = wrap0 B.true
ones = wrap1 B.ones
one = wrap1 B.one
unsignedInt = wrap2 B.unsignedInt
int = wrap2 B.int
not = wrap1 B.not
neg = wrap1 B.neg
redor = wrap1 B.redor
redxor = wrap1 B.redxor
redand = wrap1 B.redand
slice = wrap3 B.slice
uext = wrap2 B.uext
sext = wrap2 B.sext
implies = wrap2 B.implies
iff = wrap2 B.iff
xor = wrap2 B.xor
xnor = wrap2 B.xnor
(&&) = wrap2 B.and
nand = wrap2 B.nand
(||) = wrap2 B.or
nor = wrap2 B.nor
eq = wrap2 B.eq
ne = wrap2 B.ne
add = wrap2 B.add
uaddo = wrap2 B.uaddo
saddo = wrap2 B.saddo
mul = wrap2 B.mul
umulo = wrap2 B.umulo
smulo = wrap2 B.smulo
ult = wrap2 B.ult
slt  = wrap2 B.slt 
ulte  = wrap2 B.ulte 
slte  = wrap2 B.slte 
ugt  = wrap2 B.ugt 
sgt  = wrap2 B.sgt 
ugte = wrap2 B.ugte
sgte = wrap2 B.sgte
sll  = wrap2 B.sll 
srl  = wrap2 B.srl 
sra  = wrap2 B.sra 
rol  = wrap2 B.rol 
ror  = wrap2 B.ror 
sub  = wrap2 B.sub 
usubo = wrap2 B.usubo
ssubo = wrap2 B.ssubo
udiv  = wrap2 B.udiv 
sdiv  = wrap2 B.sdiv 
sdivo = wrap2 B.sdivo
urem  = wrap2 B.urem 
srem  = wrap2 B.srem 
smod  = wrap2 B.smod 
concat  = wrap2 B.concat 
read  = wrap2 B.read 
write = wrap3 B.write
cond = wrap3 B.cond

wrap_dump0 action f = wrap0 $ \ b -> do
    fp <- B.fopen f "w" ; action b fp
wrap_dump1 action f x1 = wrap0 $ \ b -> do
    fp <- B.fopen f "w" ; action b fp x1
    
dumpBtor = wrap_dump0 B.dumpBtor 
dumpBtorNode = wrap_dump1 B.dumpBtorNode
dumpSmt1 = wrap_dump0 B.dumpSmt1 
dumpSmt1Node = wrap_dump1 B.dumpSmt1Node
dumpSmt2 = wrap_dump0 B.dumpSmt2 
dumpSmt2Node = wrap_dump1 B.dumpSmt2Node

