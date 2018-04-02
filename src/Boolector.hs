{-|


This module presents Boolector functions via a monadic API similar to Z3's.

Typical usage:

@

@

TODO: this library leaks memory like a firehose, make it less so

-}

{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}

module Boolector ( -- * Boolector monadic computations
                   Boolector
                 , evalBoolector
                 , evalBoolectorTimeout
                  -- ** Options and configurations
                 , Option(..)
                 , setOpt
                 , getOpt
                 , SatSolver(..)
                 , setSatSolver
                 -- * SAT/SMT queries
                 , Node
                 , sat
                 , limitedSat
                 , simplify
                 , Status(..)
                 -- ** Assert and assume
                 , assert
                 , assume
                 , failed
                 , fixateAssumptions
                 , resetAssumptions
                 -- ** Variables and constants
                 , var
                 , const
                 -- *** Booleans
                 , bool
                 , true
                 , false
                 -- *** Bit-vectors
                 , zero
                 , one
                 , ones
                 , unsignedInt
                 , signedInt
                 -- *** Arrays
                 , array
                 -- *** Functions
                 , fun
                 , uf
                 -- **** Parameters
                 , param
                 -- ** Operations
                 -- *** Implications and conditionals
                 , implies
                 , iff
                 , cond
                 -- *** Equality checking
                 , eq
                 , ne
                 -- *** Bit flipping, extraction, extension, and reduction
                 , not
                 , neg
                 , redor
                 , redxor
                 , redand
                 , slice
                 , uext
                 , sext
                 , concat
                 -- *** Bit-wise operations
                 , xor
                 , xnor
                 , and
                 , nand
                 , or
                 , nor
                 , sll
                 , srl
                 , sra
                 , rol
                 , ror
                 -- *** Arithmetic operations
                 , add
                 , uaddo
                 , saddo
                 , inc
                 , sub
                 , usubo
                 , ssubo
                 , dec
                 , mul
                 , umulo
                 , smulo
                 , udiv
                 , sdiv
                 , sdivo
                 , urem
                 , srem
                 , smod
                 -- *** Comparison operations
                 , ult
                 , slt
                 , ulte
                 , slte
                 , ugt
                 , sgt
                 , ugte
                 , sgte
                 -- *** Array operations
                 , read
                 , write
                 -- *** Function operations
                 , apply
                 -- ** Accessors
                 , getSort
                 , funGetDomainSort
                 , funGetCodomainSort
                 , getSymbol
                 , getWidth
                 , getIndexWidth
                 , isConst
                 , isVar
                 , isArrayVar
                 , isParam
                 , isBoundParam
                 , isUf
                 , isFun
                 -- ** Models
                 , bvAssignment
                 , unsignedBvAssignment
                 , signedBvAssignment
                 , boolAssignment
                 -- ** Sorts
                 , Sort
                 , boolSort
                 , bitvecSort
                 , funSort
                 , arraySort
                 -- *** Accessors
                 , isEqualSort
                 , isArraySort
                 , isBitvecSort
                 , isFunSort
                 -- * Debug dumping
                 , dumpBtorNode
                 , dumpSmt2Node
                 , dumpBtor
                 , dumpSmt2
                 ) where

import Boolector.Foreign (Option(..), Status(..), Node, Sort)
import qualified Boolector.Foreign as B

import Data.Char (isDigit)
import Control.Monad.State.Strict
import Control.Exception hiding (assert)
import Control.Concurrent

import Prelude hiding (read, not, and, or, const, concat)
import qualified Prelude as Prelude

--
-- Boolector monad
--

-- | Solver state
newtype BoolectorState = BoolectorState { unBoolectorState :: B.Btor }


newtype Boolector a = Boolector { unBoolector :: StateT BoolectorState IO a }
    deriving (Functor, Applicative, Monad, MonadState BoolectorState, MonadIO)

-- | Like 'evalBoolector' but set a timeout in milliseconds.
evalBoolectorTimeout :: Int -> Boolector a -> IO a
evalBoolectorTimeout time action = do
  term <- newMVar 0
  bracket (createBoolectorState term) deleteBoolectorState $ \btorState -> do
    -- thread A: execute boolector formula
    void $ forkIO $ do threadDelay $ time * 1000
                       putMVar term 1 -- this will cause boolector eval to fail if not done
    evalStateT (unBoolector action) btorState
      `onException` fail "boolector timed out"
    where createBoolectorState term = do
            btorState@(BoolectorState b) <- newBoolectorState
            B.setTerm b $ \_ -> do
              readMVar term
            return btorState
          deleteBoolectorState btorState = B.delete (unBoolectorState btorState)

-- | Evaluate a Boolector action with default configurations.
evalBoolector :: Boolector a -> IO a
evalBoolector action = do
  btorState <- newBoolectorState
  evalStateT (unBoolector action) btorState
    `finally` B.delete (unBoolectorState btorState)

-- | Create new Boolector state
newBoolectorState :: IO BoolectorState
newBoolectorState = do
  b <- B.new
  B.setOpt b BTOR_OPT_MODEL_GEN 2
  B.setOpt b BTOR_OPT_AUTO_CLEANUP 1
  return $ BoolectorState b

-- | Set option. See btortypes.h
setOpt :: Option -> Int -> Boolector ()
setOpt = liftBoolector2 B.setOpt

-- | Get option. See btortypes.h
getOpt :: Option -> Boolector Int
getOpt = liftBoolector1 B.getOpt

-- | Which sat solver to use.
data SatSolver = Lingeling
               | PicoSAT
               | MiniSAT
               deriving Show

-- | Set the SAT solver to use. Returns 'True' if sucessfull.
setSatSolver :: SatSolver -> Boolector Bool
setSatSolver solver = do
  i <- liftBoolector1 B.setSatSolver (show solver)
  return (i /= 0)

-- | Add a constraint.
assert :: Node -> Boolector ()
assert = liftBoolector1 B.assert

-- | Add an assumption.
assume :: Node -> Boolector ()
assume = liftBoolector1 B.assume

-- | Determine if assumption node is a failed assumption.
failed :: Node -> Boolector Bool
failed = liftBoolector1 B.failed

-- | Add all assumptions as assertions.
fixateAssumptions :: Boolector ()
fixateAssumptions = liftBoolector0 B.fixateAssumptions

-- | Resets all added assumptions.
resetAssumptions :: Boolector ()
resetAssumptions = liftBoolector0 B.resetAssumptions

-- | Solve an input formula.
sat :: Boolector Status
sat = liftBoolector0 B.sat

-- | Solve an input formula and limit the search by the number of lemmas
-- generated and the number of conflicts encountered by the underlying
-- SAT solver.
limitedSat :: Int -- ^ Limit for lemmas on demand (-1 unlimited).
           -> Int -- ^ Conflict limit for SAT solver (-1 unlimited).
           -> Boolector Status
limitedSat = liftBoolector2 B.limitedSat

-- | Simplify current input formula.
simplify :: Boolector Status
simplify = liftBoolector0 B.sat

--
-- Expressions
--

-- | Like true and false
bool :: Bool -> Boolector Node
bool True  = true
bool False = false

-- | Create constant true. This is represented by the bit vector constant one
-- with bit width one.
true :: Boolector Node
true = liftBoolector0 B.true

-- | Create bit vector constant zero with bit width one.
false :: Boolector Node
false = liftBoolector0 B.false

-- | Create bit vector constant representing the bit vector ``bits``.
const :: String -> Boolector Node
const = liftBoolector1 B.const

-- | Create bit vector constant zero of sort ``sort``.
zero :: Sort -> Boolector Node
zero = liftBoolector1 B.zero

-- | Create bit vector constant of sort ``sort``, where each bit is set to one.
ones :: Sort -> Boolector Node
ones = liftBoolector1 B.ones

-- | Create bit vector constant one of sort ``sort``.
one :: Sort -> Boolector Node
one = liftBoolector1 B.one

-- |  Create bit vector constant representing the unsigned integer ``u`` of
-- sort ``sort``.
--
-- The constant is obtained by either truncating bits or by unsigned extension
-- (padding with zeroes).
unsignedInt :: Integer -> Sort -> Boolector Node
unsignedInt i sort = liftBoolector2 B.unsignedInt (fromIntegral i) sort

-- | Create bit vector constant representing the signed integer ``i`` of sort
-- ``sort``.
--
-- The constant is obtained by either truncating bits or by
-- signed extension (padding with ones).
signedInt :: Integer -> Sort -> Boolector Node
signedInt i sort = liftBoolector2 B.int (fromIntegral i) sort

-- | Create a bit vector variable of sort ``sort``.
var :: Sort -> String -> Boolector Node
var = liftBoolector2 B.var

-- | Create the one's complement of bit vector ``node``.
not :: Node -> Boolector Node
not = liftBoolector1 B.not

-- | Create the two's complement of bit vector ``node``.
neg :: Node -> Boolector Node
neg = liftBoolector1 B.neg

-- | Create *or* reduction of node ``node``.
--
-- All bits of node ``node`` are combined by a Boolean *or*.
redor :: Node -> Boolector Node
redor = liftBoolector1 B.redor

-- | Create *xor* reduction of node ``node``.
--
-- All bits of ``node`` are combined by a Boolean *xor*.
redxor :: Node -> Boolector Node
redxor = liftBoolector1 B.redxor

-- | Create *and* reduction of node ``node``.
--
-- All bits of ``node`` are combined by a Boolean *and*.
redand :: Node -> Boolector Node
redand = liftBoolector1 B.redand

-- | Create a bit vector slice of ``node`` from index ``upper`` to index ``lower``.
slice :: Node
      -> Int -- ^ Upper index which must be greater than or equal to zero, and less than the bit width of ``node``.
      -> Int -- ^ Lower index which must be greater than or equal to zero, and less than or equal to ``upper``.
      -> Boolector Node
slice = liftBoolector3 B.slice

-- | Create unsigned extension.
--
-- The bit vector ``node`` is padded with ``width`` * zeroes.
uext :: Node -> Int -> Boolector Node
uext = liftBoolector2 B.uext

-- | Create signed extension.
--
-- The bit vector ``node`` is padded with ``width`` bits where the value
-- depends on the value of the most significant bit of node ``n``.
sext :: Node -> Int -> Boolector Node
sext = liftBoolector2 B.sext

-- | Create the concatenation of two bit vectors.
concat :: Node -> Node -> Boolector Node
concat = liftBoolector2 B.concat

-- | Create boolean implication.
implies :: Node -> Node -> Boolector Node
implies = liftBoolector2 B.implies

-- | Create Boolean equivalence.
iff :: Node -> Node -> Boolector Node
iff = liftBoolector2 B.iff

-- | Create bit vector or array equality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
eq :: Node -> Node -> Boolector Node
eq = liftBoolector2 B.eq

-- | Create bit vector or array inequality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
ne :: Node -> Node -> Boolector Node
ne = liftBoolector2 B.ne

-- | Create an if-then-else.
--
-- If condition ``n_cond`` is true, then ``n_then`` is returned, else ``n_else``
-- is returned.
-- Nodes ``n_then`` and ``n_else`` must be either both arrays or both bit vectors.
cond :: Node -- ^ Condition
     -> Node -- ^ Then node
     -> Node -- ^ Else node
     -> Boolector Node
cond = liftBoolector3 B.cond

--
-- Bit-wise operations.
--

-- | Create a bit vector *xor*.
xor :: Node -> Node -> Boolector Node
xor = liftBoolector2 B.xor

-- | Create a bit vector *xnor*.
xnor :: Node -> Node -> Boolector Node
xnor = liftBoolector2 B.xnor

-- | Create a bit vector *and*.
and  :: Node -> Node -> Boolector Node
and  = liftBoolector2 B.and

-- | Create a bit vector *nand*.
nand :: Node -> Node -> Boolector Node
nand = liftBoolector2 B.nand

-- | Create a bit vector *or*.
or :: Node -> Node -> Boolector Node
or = liftBoolector2 B.or

-- | Create a bit vector *nor*.
nor :: Node -> Node -> Boolector Node
nor = liftBoolector2 B.nor

-- | Create a logical shift left.
--
-- Given node ``n1``, the value it represents is the number of zeroes shifted
-- into node ``n0`` from the right.
sll :: Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of ``n0``.
    -> Boolector Node
sll = liftBoolector2 B.sll

-- | Create a logical shift right.
--
-- Given node ``n1``, the value it represents is the number of zeroes shifted
-- into node ``n0`` from the left.
srl :: Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of ``n0``.
    -> Boolector Node
srl = liftBoolector2 B.srl

-- | Create an arithmetic shift right.
--
-- Analogously to 'srl', but whether zeroes or ones are shifted in depends on
-- the most significant bit of ``n0``.
sra :: Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of ``n0``.
    -> Boolector Node
sra = liftBoolector2 B.sra

-- | Create a rotate left.
--
-- Given bit vector node ``n1``, the value it represents is the number of bits
-- by which node ``n0`` is rotated to the left.
rol :: Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of ``n0``.
    -> Boolector Node
rol = liftBoolector2 B.rol

-- | Create a rotate right.
--
-- Given bit vector node ``n1``, the value it represents is the number of bits by
-- which node ``n0`` is rotated to the right.
ror :: Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of ``n0``.
    -> Boolector Node
ror = liftBoolector2 B.ror

--
-- Arithmetic operations.
--

-- | Create bit vector addition.
add :: Node -> Node -> Boolector Node
add = liftBoolector2 B.add

-- | Create an unsigned bit vector addition overflow detection.
uaddo :: Node -> Node -> Boolector Node
uaddo = liftBoolector2 B.uaddo

-- | Create a signed bit vector addition overflow detection.
saddo :: Node -> Node -> Boolector Node
saddo = liftBoolector2 B.saddo

-- | Create bit vector expression that increments bit vector ``node`` by one.
inc :: Node ->  Boolector Node
inc = liftBoolector1 B.inc

-- | Create a bit vector subtraction.
sub :: Node -> Node -> Boolector Node
sub = liftBoolector2 B.sub

-- | Create an unsigned bit vector subtraction overflow detection.
usubo :: Node -> Node -> Boolector Node
usubo = liftBoolector2 B.usubo

-- | Create a signed bit vector subtraction overflow detection.
ssubo :: Node -> Node -> Boolector Node
ssubo = liftBoolector2 B.ssubo

-- | Create bit vector expression that decrements bit vector ``node`` by one.
dec :: Node -> Boolector Node
dec = liftBoolector1 B.dec

-- | Create a bitvector multiplication.
mul :: Node -> Node -> Boolector Node
mul = liftBoolector2 B.mul

-- | Create an unsigned bit vector multiplication overflow detection.
umulo :: Node -> Node -> Boolector Node
umulo = liftBoolector2 B.umulo

-- | Create signed multiplication overflow detection.
smulo :: Node -> Node -> Boolector Node
smulo = liftBoolector2 B.smulo

-- | Create unsigned division.
udiv :: Node -> Node -> Boolector Node
udiv = liftBoolector2 B.udiv

-- | Create signed division.
sdiv :: Node -> Node -> Boolector Node
sdiv = liftBoolector2 B.sdiv

-- | Create a signed bit vector division overflow detection.
sdivo :: Node -> Node -> Boolector Node
sdivo = liftBoolector2 B.sdivo

-- | Create an unsigned remainder.
urem :: Node -> Node -> Boolector Node
urem = liftBoolector2 B.urem

-- | Create a signed remainder.
srem :: Node -> Node -> Boolector Node
srem = liftBoolector2 B.srem

-- | Create a, signed remainder where its sign matches the sign of the divisor.
smod :: Node -> Node -> Boolector Node
smod = liftBoolector2 B.smod

--
-- Comparison operations.
--

-- | Create an unsigned less than.
ult :: Node -> Node -> Boolector Node
ult = liftBoolector2 B.ult

-- | Create a signed less than.
slt :: Node -> Node -> Boolector Node
slt = liftBoolector2 B.slt

-- | Create an unsigned less than or equal.
ulte :: Node -> Node -> Boolector Node
ulte = liftBoolector2 B.ulte

-- | Create a signed less than or equal.
slte :: Node -> Node -> Boolector Node
slte = liftBoolector2 B.slte

-- | Create an unsigned greater than.
ugt :: Node -> Node -> Boolector Node
ugt = liftBoolector2 B.ugt

-- | Create a signed greater than.
sgt :: Node -> Node -> Boolector Node
sgt = liftBoolector2 B.sgt

-- | Create an unsigned greater than or equal.
ugte :: Node -> Node -> Boolector Node
ugte = liftBoolector2 B.ugte

-- | Create a signed greater than or equal.
sgte :: Node -> Node -> Boolector Node
sgte = liftBoolector2 B.sgte

--
-- Array operations
--

-- | Create a one-dimensional bit vector array with sort ``sort``.
--
-- The name must be unique.
array :: Sort -> String -> Boolector Node
array = liftBoolector2 B.array

-- | Create a read on array ``n_array`` at position ``n_index``.
read :: Node -- ^ Array operand.
     -> Node -- ^ Bit vector index. The bit width of ``n_index`` must have the same bit width as the indices of ``n_array``.
     -> Boolector Node
read = liftBoolector2 B.read

-- | Create a write on array ``n_array`` at position ``n_index`` with value
-- ``n_value``.
--
-- The array is updated at exactly one position, all other elements remain
-- unchanged. The bit width of ``n_index`` must be the same as the bit width of
-- the indices of ``n_array``. The bit width of ``n_value`` must be the same as
-- the bit width of the elements of ``n_array``.
write :: Node -- ^ Array operand.
      -> Node -- ^ Bit vector index.
      -> Node -- ^ Bit vector value.
      -> Boolector Node
write = liftBoolector3 B.write

--
-- Functions
--

-- | Create an uninterpreted function with sort ``sort``.
--
-- The name must be unique.
uf :: Sort -> String -> Boolector Node
uf = liftBoolector2 B.uf


-- | Create function parameter of sort ``sort``.
--
-- This kind of node is used to create parameterized expressions, which are
-- used to create functions. Once a parameter is bound to a function, it
-- cannot be re-used in other functions.
param :: Sort -> String -> Boolector Node
param = liftBoolector2 B.param

-- | Create a function with body ``node`` parameterized over parameters
-- ``param_nodes``.
--
-- This kind of node is similar to macros in the SMT-LIB standard 2.0.
-- Note that as soon as a parameter is bound to a function, it can not be
-- reused in other functions.
-- Call a function via 'apply'.
fun :: [Node] -- ^ Parameters of function.
    -> Node   -- ^ Function body parameterized over ``param_nodes``.
    -> Boolector Node
fun = liftBoolector2 B.fun

-- | Create a function application on function ``n_fun`` with arguments
-- ``arg_nodes``.
apply :: [Node] -- ^ Arguments to be applied.
      -> Node   -- ^ Number of arguments to be applied.
      -> Boolector Node
apply = liftBoolector2 B.apply


--
-- Accessors
--

-- | Get the sort of given ``node``. The result does not have to be released.
getSort :: Node -> Boolector Sort
getSort = liftBoolector1 B.getSort

-- | Get the domain sort of given function node ``node``.
--
-- The result does not have to be released.
funGetDomainSort :: Node -> Boolector Sort
funGetDomainSort = liftBoolector1 B.funGetDomainSort

-- | Get the codomain sort of given function node ``node``.
--
-- The result does not have to be released.
funGetCodomainSort :: Node -> Boolector Sort
funGetCodomainSort = liftBoolector1 B.funGetCodomainSort

-- | Get the symbol of an expression.
getSymbol :: Node -> Boolector String
getSymbol = liftBoolector1 B.getSymbol

-- | Get the bit width of an expression.
--
-- If the expression is an array, it returns the bit width of the array
-- elements.
-- If the expression is a function, it returns the bit width of the function's
-- return value.
getWidth :: Node -> Boolector Int
getWidth = liftBoolector1 B.getWidth

-- | Get the bit width of indices of ``n_array``.
getIndexWidth :: Node -> Boolector Int
getIndexWidth = liftBoolector1 B.getIndexWidth

-- | Determine if given node is a constant node.
isConst :: Node -> Boolector Bool
isConst = liftBoolector1 B.isConst

-- | Determine if given node is a bit vector variable.
isVar :: Node -> Boolector Bool
isVar = liftBoolector1 B.isVar

-- | Determine if given node is an array node.
isArrayVar :: Node -> Boolector Bool
isArrayVar = liftBoolector1 B.isArrayVar

-- | Determine if given node is a parameter node.
isParam :: Node -> Boolector Bool
isParam = liftBoolector1 B.isParam

-- | Determine if given parameter node is bound by a function.
isBoundParam :: Node -> Boolector Bool
isBoundParam = liftBoolector1 B.isBoundParam

-- | Determine if given node is an uninterpreted function node.
isUf :: Node -> Boolector Bool
isUf = liftBoolector1 B.isUf

-- | Determine if given node is a function node.
isFun :: Node -> Boolector Bool
isFun = liftBoolector1 B.isFun


--
-- Models.
--

-- | Generate an assignment string for bit vector expression if
-- boolector_sat has returned BOOLECTOR_SAT and model generation has been
-- enabled.
--
-- The expression can be an arbitrary bit vector expression which
-- occurs in an assertion or current assumption. The assignment string has to
-- be freed by 'freeBvAssignment'.
bvAssignment :: Node -> Boolector String
bvAssignment = liftBoolector1 B.bvAssignment

-- | Get unsigned integer value from model.
unsignedBvAssignment :: Node -> Boolector Integer
unsignedBvAssignment node = do
  str <- bvAssignment node
  when (Prelude.not $ all isDigit str) $ error $ "getModelVal: not numeric: " ++ str
  liftIO $ evaluate $ foldl (\ n c -> 2 * n + Prelude.read [c]) 0 str

-- | Get signed integer value from model.
signedBvAssignment :: Node -> Boolector Integer
signedBvAssignment node = do
    val <- unsignedBvAssignment node
    w <- getWidth node
    let max_signed_w = 2 ^ pred w
    return $ if val >= max_signed_w
                then val - (2*max_signed_w)
                else val

-- | Get Boolean value from model.
boolAssignment :: Node -> Boolector Bool
boolAssignment node = do
    str <- bvAssignment node
    liftIO $ evaluate $ case str of
        "0" -> False
        "1" -> True
        _   -> error $ "boolAssignment: not boolean: " ++ str

--
-- Sorts
--


-- | Create Boolean sort.
boolSort :: Boolector Sort
boolSort = liftBoolector0 B.boolSort

-- | Create bit vector sort of bit width ``width``.
bitvecSort :: Int -> Boolector Sort
bitvecSort = liftBoolector1 B.bitvecSort

-- | Create function sort.
funSort :: [Sort] -> Sort -> Boolector Sort
funSort = liftBoolector2 B.funSort

-- | Create array sort.
arraySort :: Sort -> Sort -> Boolector Sort
arraySort = liftBoolector2 B.arraySort

-- | Determine if ``n0`` and ``n1`` have the same sort or not.
isEqualSort :: Node -> Node -> Boolector Bool
isEqualSort = liftBoolector2 B.isEqualSort

-- | Determine if ``sort`` is an array sort.
isArraySort :: Sort -> Boolector Bool
isArraySort = liftBoolector1 B.isArraySort

-- | Determine if ``sort`` is a bit-vector sort.
isBitvecSort :: Sort -> Boolector Bool
isBitvecSort = liftBoolector1 B.isBitvecSort

-- | Determine if ``sort`` is a function sort.
isFunSort :: Sort -> Boolector Bool
isFunSort = liftBoolector1 B.isFunSort


--
-- Dumping
--

-- | Recursively dump ``node`` to file in BTOR_ format.
dumpBtorNode :: FilePath -> Node -> Boolector ()
dumpBtorNode path node = do
  file <- liftIO $ B.fopen path "w"
  liftBoolector2 B.dumpBtorNode file node

-- | Recursively dump ``node`` to file in `SMT-LIB v2`_ format.
dumpSmt2Node :: FilePath -> Node -> Boolector ()
dumpSmt2Node path node = do
  file <- liftIO $ B.fopen path "w"
  liftBoolector2 B.dumpSmt2Node file node

-- | Dump formula to file in BTOR_ format.
dumpBtor :: FilePath -> Boolector ()
dumpBtor path = do
  file <- liftIO $ B.fopen path "w"
  liftBoolector1 B.dumpBtor file

-- | Dumps formula to file in `SMT-LIB v2`_ format.
dumpSmt2 :: FilePath -> Boolector ()
dumpSmt2 path = do
  file <- liftIO $ B.fopen path "w"
  liftBoolector1 B.dumpSmt2 file

--
-- Helpers
--

liftBoolector0 :: (B.Btor -> IO a) -> Boolector a
liftBoolector0 f = do
  s <- get
  liftIO $ f (unBoolectorState s)

liftBoolector1 :: (B.Btor -> a -> IO b) -> a -> Boolector b
liftBoolector1 f x1 = do
  s <- get
  liftIO $ f (unBoolectorState s) x1

liftBoolector2 :: (B.Btor -> a -> b -> IO c) -> a -> b -> Boolector c
liftBoolector2 f x1 x2 = do
  s <- get
  liftIO $ f (unBoolectorState s) x1 x2

liftBoolector3 :: (B.Btor -> a -> b -> c -> IO d) -> a -> b -> c -> Boolector d
liftBoolector3 f x1 x2 x3 = do
  s <- get
  liftIO $ f (unBoolectorState s) x1 x2 x3
