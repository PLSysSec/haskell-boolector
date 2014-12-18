{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}

module Boolector

( module Boolector
, Node ()
)       

where

import qualified Boolector.Foreign as B
import Boolector.Foreign (Node)

import Control.Monad.State
import Control.Applicative
import Data.Char (isDigit)

import Prelude hiding (read, not, and, or, (&&), (||))
import qualified Prelude

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
    B.setOpt b "model_gen" 2
    let s0 = S { btor = b , next = 0, solved = False }
    cont <- evalStateT (un action) s0
    status <- B.sat b
    case status of
        B.SAT -> do
            Just <$> evalStateT (un cont) s0 { solved = True }
        _ -> return Nothing

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

