{-|

This module exposes a DSL for writing symbolic computations atop the Boolector
SMT solver. The monadic interface manages the interface to Boolector, caches
already created sorts and variables, etc. A Boolector computation should not be
shared between threads.

Consider, the simple example from the Z3 tutorial
<https://rise4fun.com/z3/tutorialcontent/guide#h23> written in SMT LIB format:

@
  (declare-fun f (Int) Int)
  (declare-fun a () Int) ; a is a constant
  (declare-const b Int) ; syntax sugar for (declare-fun b () Int)
  (assert (> a 20))
  (assert (> b a))
  (assert (= (f 10) 1))
  (check-sat)
  (get-model)
@

With this library you can write the same program in Haskell:

@
main :: IO ()
main = do
  bs <- B.'newBoolectorState' Nothing
  B.'evalBoolector' bs $ do
    -- Create sorts:
    u32 <- B.'bitvecSort' 32
    fSort <- B.'funSort' [u32] u32

    -- Create variables f, a, and b:
    f <- B.'uf' fSort "f"
    a <- B.'var' u32 "a"
    b <- B.'var' u32 "b"

    -- Create several constants:
    c20 <- B.'unsignedInt' 20 u32
    c10 <- B.'unsignedInt' 10 u32
    c1  <- B.'one' u32

    -- Make assertions:
    B.'assert' =<< B.'ugt' a c20
    B.'assert' =<< B.'ugt' b a

    res <- B.'apply' [c10] f
    B.'assert' =<< B.'eq' res c1

    -- Check satisfiability:
    B.'Sat' <- B.'sat'

    -- Get model:
    ma  <- B.'unsignedBvAssignment' a
    mb  <- B.'unsignedBvAssignment' b

    -- Check model:
    assert (ma == 21) $ return ()
    assert (mb == 22) $ return ()
@

The API is inspired by the Z3 Haskell API <http://hackage.haskell.org/package/z3>.

-}

{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}

module Boolector ( -- * Boolector monadic computations
                   Boolector
                 , MonadBoolector(..)
                 , evalBoolector
                 , runBoolector
                  -- ** Boolector state
                 , BoolectorState
                 , newBoolectorState
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
                 , push
                 , pop
                 -- ** Variables and constants
                 , var
                 , const
                 , constd
                 , consth
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
                 -- *** Quantified terms
                 , forall
                 , exists
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
                 , repeat
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
                 , funGetArity
                 , getSymbol
                 , setSymbol
                 , getWidth
                 , getIndexWidth
                 , isConst
                 , isVar
                 , isArray
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
                 , funSortCheck
                 -- * Debug dumping
                 , dump
                 , dumpNode
                 , dumpToString
                 , dumpNodeToString
                 , DumpFormat(..)
                 ) where

import Boolector.Foreign (Option(..), Status(..), Node, Sort)
import qualified Boolector.Foreign as B

import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word

import Control.Monad.State.Strict
import Control.Exception hiding (assert)
import Control.Concurrent

import Prelude hiding (read, not, and, or, const, concat, repeat)
import qualified Prelude as Prelude

--
-- Boolector monad
--

-- | Type class for Monads that wish to perform symbolic computations.
class MonadIO m => MonadBoolector m where
  -- | Get the Boolector state.
  getBoolectorState :: m BoolectorState
  -- | Put the Boolector state.
  putBoolectorState :: BoolectorState -> m ()

instance MonadBoolector Boolector where
  getBoolectorState = get
  putBoolectorState = put

-- | Solver state and cache
data BoolectorState = BoolectorState { unBoolectorState :: B.Btor
                                     , unBoolectorCache :: BoolectorCache }

-- | Bolector monad, keeping track of underlying solver state.
newtype Boolector a = Boolector { unBoolector :: StateT BoolectorState IO a }
    deriving (Functor, Applicative, Monad, MonadState BoolectorState, MonadIO)

-- | Evaluate a Boolector action with a given configurations.
evalBoolector :: BoolectorState -> Boolector a -> IO a
evalBoolector bState act = evalStateT (unBoolector act) bState

-- | Like 'evalBoolector', but take an explicit starting BoolectorState, and
-- return the final BoolectorState
runBoolector :: BoolectorState -> Boolector a -> IO (a, BoolectorState)
runBoolector bState act = runStateT (unBoolector act) bState

-- | Create new Boolector state with optional timeout. By default, we enable
-- support for model generation and incremental solving.
newBoolectorState :: Maybe Int -> IO BoolectorState
newBoolectorState Nothing = do
  b <- B.new
  B.setOpt b OPT_MODEL_GEN 2
  B.setOpt b OPT_AUTO_CLEANUP 1
  B.setOpt b OPT_INCREMENTAL 1
  return $ BoolectorState b emptyBoolectorCache
newBoolectorState (Just time) = do
  term <- newMVar 0
  btorState@(BoolectorState b _) <- newBoolectorState Nothing
  B.setTerm b $ \_ -> do
    readMVar term
  void $ forkIO $ do threadDelay $ time * 1000
                     putMVar term 1 -- this will cause boolector eval to fail if not done
  return btorState

-- | Set option.
setOpt :: MonadBoolector m => Option -> Word32 -> m ()
setOpt o w = liftBoolector2 B.setOpt o (fromIntegral w)

-- | Get option.
getOpt :: MonadBoolector m => Option -> m Word32
getOpt o = fromIntegral `liftM` liftBoolector1 B.getOpt o

-- | Which sat solver to use.
data SatSolver = Lingeling
               | PicoSAT
               | MiniSAT
               deriving Show

-- | Set the SAT solver to use. Returns 'True' if sucessfull.
setSatSolver :: MonadBoolector m => SatSolver -> m ()
setSatSolver solver = liftBoolector1 B.setSatSolver (show solver)

-- | Add a constraint.
assert :: MonadBoolector m => Node -> m ()
assert = liftBoolector1 B.assert

-- | Add an assumption.
assume :: MonadBoolector m => Node -> m ()
assume = liftBoolector1 B.assume

-- | Determine if assumption node is a failed assumption.
failed :: MonadBoolector m => Node -> m Bool
failed = liftBoolector1 B.failed

-- | Add all assumptions as assertions.
fixateAssumptions :: MonadBoolector m => m ()
fixateAssumptions = liftBoolector0 B.fixateAssumptions

-- | Resets all added assumptions.
resetAssumptions :: MonadBoolector m => m ()
resetAssumptions = liftBoolector0 B.resetAssumptions

-- | Solve an input formula.
sat :: MonadBoolector m => m Status
sat = liftBoolector0 B.sat

-- | Push new context levels.
push :: MonadBoolector m => Word32 -> m ()
push w = liftBoolector1 B.push (fromIntegral w)

-- | Pop context levels.
pop :: MonadBoolector m => Word32 -> m ()
pop w = liftBoolector1 B.pop (fromIntegral w)

-- | Solve an input formula and limit the search by the number of lemmas
-- generated and the number of conflicts encountered by the underlying
-- SAT solver.
limitedSat :: MonadBoolector m
           => Int -- ^ Limit for lemmas on demand (-1 unlimited).
           -> Int -- ^ Conflict limit for SAT solver (-1 unlimited).
           -> m Status
limitedSat = liftBoolector2 B.limitedSat

-- | Simplify current input formula.
simplify :: MonadBoolector m => m Status
simplify = liftBoolector0 B.sat

--
-- Expressions
--

-- | Like true and false
bool :: MonadBoolector m => Bool -> m Node
bool True  = true
bool False = false

-- | Create constant true. This is represented by the bit vector constant one
-- with bit width one.
true :: MonadBoolector m => m Node
true = liftBoolector0 B.true

-- | Create bit vector constant zero with bit width one.
false :: MonadBoolector m => m Node
false = liftBoolector0 B.false

-- | Create bit vector constant representing the bit vector @bits@.
const :: MonadBoolector m => String -> m Node
const = liftBoolector1 B.const

-- | Create bit vector constant representing the decimal number @str@.
constd :: MonadBoolector m => Sort -> String -> m Node
constd = liftBoolector2 B.constd

-- | Create bit vector constant representing the hexadecimal number @str@.
consth :: MonadBoolector m => Sort -> String -> m Node
consth = liftBoolector2 B.consth

-- | Create bit vector constant zero of sort @sort@.
zero :: MonadBoolector m => Sort -> m Node
zero = liftBoolector1 B.zero

-- | Create bit vector constant of sort @sort@, where each bit is set to one.
ones :: MonadBoolector m => Sort -> m Node
ones = liftBoolector1 B.ones

-- | Create bit vector constant one of sort @sort@.
one :: MonadBoolector m => Sort -> m Node
one = liftBoolector1 B.one

-- |  Create bit vector constant representing the unsigned integer @u@ of
-- sort @sort@.
--
-- The constant is obtained by either truncating bits or by unsigned extension
-- (padding with zeroes).
unsignedInt :: MonadBoolector m => Integer -> Sort -> m Node
unsignedInt i sort = liftBoolector2 B.unsignedInt (fromIntegral i) sort

-- | Create bit vector constant representing the signed integer @i@ of sort
-- @sort@.
--
-- The constant is obtained by either truncating bits or by
-- signed extension (padding with ones).
signedInt :: MonadBoolector m => Integer -> Sort -> m Node
signedInt i sort = liftBoolector2 B.int (fromIntegral i) sort

-- | Create a bit vector variable of sort @sort@.
var :: MonadBoolector m => Sort -> String -> m Node
var = createNamedNode B.var

-- | Create the one's complement of bit vector @node@.
not :: MonadBoolector m => Node -> m Node
not = liftBoolector1 B.not

-- | Create the two's complement of bit vector @node@.
neg :: MonadBoolector m => Node -> m Node
neg = liftBoolector1 B.neg

-- | Create *or* reduction of node @node@.
--
-- All bits of node @node@ are combined by a Boolean *or*.
redor :: MonadBoolector m => Node -> m Node
redor = liftBoolector1 B.redor

-- | Create *xor* reduction of node @node@.
--
-- All bits of @node@ are combined by a Boolean *xor*.
redxor :: MonadBoolector m => Node -> m Node
redxor = liftBoolector1 B.redxor

-- | Create *and* reduction of node @node@.
--
-- All bits of @node@ are combined by a Boolean *and*.
redand :: MonadBoolector m => Node -> m Node
redand = liftBoolector1 B.redand

-- | Create a bit vector slice of @node@ from index @upper@ to index @lower@.
slice :: MonadBoolector m
      => Node -- ^ Bit vector node.
      -> Word32 -- ^ Upper index which must be greater than or equal to zero, and less than the bit width of @node@.
      -> Word32 -- ^ Lower index which must be greater than or equal to zero, and less than or equal to @upper@.
      -> m Node
slice n u l = (liftBoolector3 B.slice) n (fromIntegral u) (fromIntegral l)

-- | Create unsigned extension.
--
-- The bit vector @node@ is padded with @width@ * zeroes.
uext :: MonadBoolector m => Node -> Word32 -> m Node
uext n w = (liftBoolector2 B.uext) n $ fromIntegral w

-- | Create signed extension.
--
-- The bit vector @node@ is padded with @width@ bits where the value
-- depends on the value of the most significant bit of node @n@.
sext :: MonadBoolector m => Node -> Word32 -> m Node
sext n w = liftBoolector2 B.sext n (fromIntegral w)

-- | Create the concatenation of two bit vectors.
concat :: MonadBoolector m => Node -> Node -> m Node
concat = liftBoolector2 B.concat

-- | Create @n@ concatenations of a given node @node@.
repeat :: MonadBoolector m => Node -> Word32 -> m Node
repeat n w = liftBoolector2 B.repeat n (fromIntegral w)

-- | Create boolean implication.
implies :: MonadBoolector m => Node -> Node -> m Node
implies = liftBoolector2 B.implies

-- | Create Boolean equivalence.
iff :: MonadBoolector m => Node -> Node -> m Node
iff = liftBoolector2 B.iff

-- | Create bit vector or array equality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
eq :: MonadBoolector m => Node -> Node -> m Node
eq = liftBoolector2 B.eq

-- | Create bit vector or array inequality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
ne :: MonadBoolector m => Node -> Node -> m Node
ne = liftBoolector2 B.ne

-- | Create an if-then-else.
--
-- If condition @n_cond@ is true, then @n_then@ is returned, else @n_else@
-- is returned.
-- Nodes @n_then@ and @n_else@ must be either both arrays or both bit vectors.
cond :: MonadBoolector m
     => Node -- ^ Condition
     -> Node -- ^ Then node
     -> Node -- ^ Else node
     -> m Node
cond = liftBoolector3 B.cond

--
-- Bit-wise operations.
--

-- | Create a bit vector *xor*.
xor :: MonadBoolector m => Node -> Node -> m Node
xor = liftBoolector2 B.xor

-- | Create a bit vector *xnor*.
xnor :: MonadBoolector m => Node -> Node -> m Node
xnor = liftBoolector2 B.xnor

-- | Create a bit vector *and*.
and  :: MonadBoolector m => Node -> Node -> m Node
and  = liftBoolector2 B.and

-- | Create a bit vector *nand*.
nand :: MonadBoolector m => Node -> Node -> m Node
nand = liftBoolector2 B.nand

-- | Create a bit vector *or*.
or :: MonadBoolector m => Node -> Node -> m Node
or = liftBoolector2 B.or

-- | Create a bit vector *nor*.
nor :: MonadBoolector m => Node -> Node -> m Node
nor = liftBoolector2 B.nor

-- | Create a logical shift left.
--
-- Given node @n1@, the value it represents is the number of zeroes shifted
-- into node @n0@ from the right.
sll :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
sll = liftBoolector2 B.sll

-- | Create a logical shift right.
--
-- Given node @n1@, the value it represents is the number of zeroes shifted
-- into node @n0@ from the left.
srl :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
srl = liftBoolector2 B.srl

-- | Create an arithmetic shift right.
--
-- Analogously to 'srl', but whether zeroes or ones are shifted in depends on
-- the most significant bit of @n0@.
sra :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
sra = liftBoolector2 B.sra

-- | Create a rotate left.
--
-- Given bit vector node @n1@, the value it represents is the number of bits
-- by which node @n0@ is rotated to the left.
rol :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
rol = liftBoolector2 B.rol

-- | Create a rotate right.
--
-- Given bit vector node @n1@, the value it represents is the number of bits by
-- which node @n0@ is rotated to the right.
ror :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
ror = liftBoolector2 B.ror

--
-- Arithmetic operations.
--

-- | Create bit vector addition.
add :: MonadBoolector m => Node -> Node -> m Node
add = liftBoolector2 B.add

-- | Create an unsigned bit vector addition overflow detection.
uaddo :: MonadBoolector m => Node -> Node -> m Node
uaddo = liftBoolector2 B.uaddo

-- | Create a signed bit vector addition overflow detection.
saddo :: MonadBoolector m => Node -> Node -> m Node
saddo = liftBoolector2 B.saddo

-- | Create bit vector expression that increments bit vector @node@ by one.
inc :: Node ->  Boolector Node
inc = liftBoolector1 B.inc

-- | Create a bit vector subtraction.
sub :: MonadBoolector m => Node -> Node -> m Node
sub = liftBoolector2 B.sub

-- | Create an unsigned bit vector subtraction overflow detection.
usubo :: MonadBoolector m => Node -> Node -> m Node
usubo = liftBoolector2 B.usubo

-- | Create a signed bit vector subtraction overflow detection.
ssubo :: MonadBoolector m => Node -> Node -> m Node
ssubo = liftBoolector2 B.ssubo

-- | Create bit vector expression that decrements bit vector @node@ by one.
dec :: MonadBoolector m => Node -> m Node
dec = liftBoolector1 B.dec

-- | Create a bitvector multiplication.
mul :: MonadBoolector m => Node -> Node -> m Node
mul = liftBoolector2 B.mul

-- | Create an unsigned bit vector multiplication overflow detection.
umulo :: MonadBoolector m => Node -> Node -> m Node
umulo = liftBoolector2 B.umulo

-- | Create signed multiplication overflow detection.
smulo :: MonadBoolector m => Node -> Node -> m Node
smulo = liftBoolector2 B.smulo

-- | Create unsigned division.
udiv :: MonadBoolector m => Node -> Node -> m Node
udiv = liftBoolector2 B.udiv

-- | Create signed division.
sdiv :: MonadBoolector m => Node -> Node -> m Node
sdiv = liftBoolector2 B.sdiv

-- | Create a signed bit vector division overflow detection.
sdivo :: MonadBoolector m => Node -> Node -> m Node
sdivo = liftBoolector2 B.sdivo

-- | Create an unsigned remainder.
urem :: MonadBoolector m => Node -> Node -> m Node
urem = liftBoolector2 B.urem

-- | Create a signed remainder.
srem :: MonadBoolector m => Node -> Node -> m Node
srem = liftBoolector2 B.srem

-- | Create a, signed remainder where its sign matches the sign of the divisor.
smod :: MonadBoolector m => Node -> Node -> m Node
smod = liftBoolector2 B.smod

--
-- Comparison operations.
--

-- | Create an unsigned less than.
ult :: MonadBoolector m => Node -> Node -> m Node
ult = liftBoolector2 B.ult

-- | Create a signed less than.
slt :: MonadBoolector m => Node -> Node -> m Node
slt = liftBoolector2 B.slt

-- | Create an unsigned less than or equal.
ulte :: MonadBoolector m => Node -> Node -> m Node
ulte = liftBoolector2 B.ulte

-- | Create a signed less than or equal.
slte :: MonadBoolector m => Node -> Node -> m Node
slte = liftBoolector2 B.slte

-- | Create an unsigned greater than.
ugt :: MonadBoolector m => Node -> Node -> m Node
ugt = liftBoolector2 B.ugt

-- | Create a signed greater than.
sgt :: MonadBoolector m => Node -> Node -> m Node
sgt = liftBoolector2 B.sgt

-- | Create an unsigned greater than or equal.
ugte :: MonadBoolector m => Node -> Node -> m Node
ugte = liftBoolector2 B.ugte

-- | Create a signed greater than or equal.
sgte :: MonadBoolector m => Node -> Node -> m Node
sgte = liftBoolector2 B.sgte

--
-- Array operations
--

-- | Create a one-dimensional bit vector array with sort @sort@.
--
-- The name must be unique.
array :: MonadBoolector m => Sort -> String -> m Node
array = createNamedNode B.array

-- | Create a read on array @n_array@ at position @n_index@.
read :: MonadBoolector m
     => Node -- ^ Array operand.
     -> Node -- ^ Bit vector index. The bit width of @n_index@ must have the same bit width as the indices of @n_array@.
     -> m Node
read = liftBoolector2 B.read

-- | Create a write on array @n_array@ at position @n_index@ with value
-- @n_value@.
--
-- The array is updated at exactly one position, all other elements remain
-- unchanged. The bit width of @n_index@ must be the same as the bit width of
-- the indices of @n_array@. The bit width of @n_value@ must be the same as
-- the bit width of the elements of @n_array@.
write :: MonadBoolector m
      => Node -- ^ Array operand.
      -> Node -- ^ Bit vector index.
      -> Node -- ^ Bit vector value.
      -> m Node
write = liftBoolector3 B.write

--
-- Functions
--

-- | Create an uninterpreted function with sort @sort@.
--
-- The name must be unique.
uf :: MonadBoolector m => Sort -> String -> m Node
uf = createNamedNode B.uf

-- | Create function parameter of sort @sort@.
--
-- This kind of node is used to create parameterized expressions, which are
-- used to create functions. Once a parameter is bound to a function, it
-- cannot be re-used in other functions.
param :: MonadBoolector m => Sort -> String -> m Node
param = liftBoolector2 B.param

-- | Create a function with body @node@ parameterized over parameters
-- @param_nodes@.
--
-- This kind of node is similar to macros in the SMT-LIB standard 2.0.
-- Note that as soon as a parameter is bound to a function, it can not be
-- reused in other functions.
-- Call a function via 'apply'.
fun :: MonadBoolector m
    => [Node] -- ^ Parameters of function.
    -> Node   -- ^ Function body parameterized over @param_nodes@.
    -> m Node
fun = liftBoolector2 B.fun

-- | Create a function application on function @n_fun@ with arguments
-- @arg_nodes@.
apply :: MonadBoolector  m
      => [Node] -- ^ Arguments to be applied.
      -> Node   -- ^ Number of arguments to be applied.
      -> m Node
apply = liftBoolector2 B.apply


--
-- Quantified terms
--

-- | Create a universally quantified term.
forall :: MonadBoolector m
       => [Node] -- ^ Quantified variables
       -> Node   -- ^ Term where variables may occur
       -> m Node
forall = liftBoolector2 B.forall

-- | Create an existentially quantifed term.
exists :: MonadBoolector m
       => [Node] -- ^ Quantified variables
       -> Node   -- ^ Term where variables may occur
       -> m Node
exists = liftBoolector2 B.exists

--
-- Accessors
--

-- | Get the sort of given @node@. The result does not have to be released.
getSort :: MonadBoolector m => Node -> m Sort
getSort = liftBoolector1 B.getSort

-- | Get the domain sort of given function node @node@.
--
-- The result does not have to be released.
funGetDomainSort :: MonadBoolector m => Node -> m Sort
funGetDomainSort = liftBoolector1 B.funGetDomainSort

-- | Get the codomain sort of given function node @node@.
--
-- The result does not have to be released.
funGetCodomainSort :: MonadBoolector m => Node -> m Sort
funGetCodomainSort = liftBoolector1 B.funGetCodomainSort

-- | Get the arity of function node.
funGetArity :: MonadBoolector m => Node -> m Word
funGetArity n = fromIntegral `liftM` liftBoolector1 B.getFunArity n

-- | Get the symbol of an expression.
getSymbol :: MonadBoolector m => Node -> m (Maybe String)
getSymbol = liftBoolector1 B.getSymbol

-- | Set the symbol of an expression.
setSymbol :: MonadBoolector m => Node -> String -> m ()
setSymbol = liftBoolector2 B.setSymbol

-- | Get the bit width of an expression.
--
-- If the expression is an array, it returns the bit width of the array
-- elements.
-- If the expression is a function, it returns the bit width of the function's
-- return value.
getWidth :: MonadBoolector m => Node -> m Word
getWidth n = fromIntegral `liftM` liftBoolector1 B.getWidth n

-- | Get the bit width of indices of @n_array@.
getIndexWidth :: MonadBoolector m => Node -> m Word
getIndexWidth n = fromIntegral `liftM` liftBoolector1 B.getIndexWidth n

-- | Determine if given node is a constant node.
isConst :: MonadBoolector m => Node -> m Bool
isConst = liftBoolector1 B.isConst

-- | Determine if given node is a bit vector variable.
isVar :: MonadBoolector m => Node -> m Bool
isVar = liftBoolector1 B.isVar

-- | Determine if given node is an array node.
isArray :: MonadBoolector m => Node -> m Bool
isArray = liftBoolector1 B.isArray

-- | Determine if given node is an array node.
isArrayVar :: MonadBoolector m => Node -> m Bool
isArrayVar = liftBoolector1 B.isArrayVar

-- | Determine if given node is a parameter node.
isParam :: MonadBoolector m => Node -> m Bool
isParam = liftBoolector1 B.isParam

-- | Determine if given parameter node is bound by a function.
isBoundParam :: MonadBoolector m => Node -> m Bool
isBoundParam = liftBoolector1 B.isBoundParam

-- | Determine if given node is an uninterpreted function node.
isUf :: MonadBoolector m => Node -> m Bool
isUf = liftBoolector1 B.isUf

-- | Determine if given node is a function node.
isFun :: MonadBoolector m => Node -> m Bool
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
bvAssignment :: MonadBoolector m => Node -> m String
bvAssignment = liftBoolector1 B.bvAssignment

-- | Get unsigned integer value from model.
unsignedBvAssignment :: MonadBoolector m => Node -> m Integer
unsignedBvAssignment node = do
  str <- bvAssignment node
  when (Prelude.not $ all isDigit str) $ error $ "getModelVal: not numeric: " ++ str
  liftIO $ evaluate $ foldl (\ n c -> 2 * n + Prelude.read [c]) 0 str

-- | Get signed integer value from model.
signedBvAssignment :: MonadBoolector m => Node -> m Integer
signedBvAssignment node = do
    val <- unsignedBvAssignment node
    w <- getWidth node
    let max_signed_w = 2 ^ pred w
    return $ if val >= max_signed_w
                then val - (2*max_signed_w)
                else val

-- | Get Boolean value from model.
boolAssignment :: MonadBoolector m => Node -> m Bool
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
boolSort = do
  sc <- getSortCache
  case scBool sc of
    Just srt -> return srt
    _ -> do srt <- liftBoolector0 B.boolSort
            setSortCache $ sc { scBool = Just srt }
            return srt

-- | Create bit vector sort of bit width @width@.
bitvecSort :: MonadBoolector m => Word -> m Sort
bitvecSort wnr = do
  sc <- getSortCache
  let bvMap = scBitVec sc
  case IntMap.lookup nr bvMap of
    Just srt -> return srt
    _ -> do srt <- liftBoolector1 B.bitvecSort nr
            setSortCache $ sc { scBitVec = IntMap.insert nr srt bvMap }
            return srt
  where nr = fromIntegral wnr

-- | Create function sort.
funSort :: MonadBoolector m => [Sort] -> Sort -> m Sort
funSort args ret = do
  sc <- getSortCache
  let funMap = scFun sc
  case Map.lookup (ret, args) funMap of
    Just srt -> return srt
    _ -> do srt <- liftBoolector2 B.funSort args ret
            setSortCache $ sc { scFun = Map.insert (ret, args) srt funMap }
            return srt

-- | Create array sort.
arraySort :: MonadBoolector m => Sort -> Sort -> m Sort
arraySort dom rng = do
  sc <- getSortCache
  let arrMap = scArray sc
  case Map.lookup (dom, rng) arrMap of
    Just srt -> return srt
    _ -> do srt <- liftBoolector2 B.arraySort dom rng
            setSortCache $ sc { scArray = Map.insert (dom, rng) srt arrMap }
            return srt

-- | Determine if @n0@ and @n1@ have the same sort or not.
isEqualSort :: MonadBoolector m => Node -> Node -> m Bool
isEqualSort = liftBoolector2 B.isEqualSort

-- | Determine if @sort@ is an array sort.
isArraySort :: MonadBoolector m => Sort -> m Bool
isArraySort = liftBoolector1 B.isArraySort

-- | Determine if @sort@ is a bit-vector sort.
isBitvecSort :: MonadBoolector m => Sort -> m Bool
isBitvecSort = liftBoolector1 B.isBitvecSort

-- | Determine if @sort@ is a function sort.
isFunSort :: MonadBoolector m => Sort -> m Bool
isFunSort = liftBoolector1 B.isFunSort

-- | Check if sorts of given arguments matches the function signature.
-- Returns 'Nothing' if all sorts are correct; otherwise it returns the
-- position of the incorrect argument.
funSortCheck :: MonadBoolector m => [Node] -> Node -> m (Maybe Int)
funSortCheck = liftBoolector2 B.funSortCheck


--
-- Dumping
--

-- | Output dump format.
data DumpFormat = DumpBtor | DumpSMT2
      deriving (Eq, Show)

-- | Recursively dump @node@ to file in BTOR or SMT-LIB v2 format.
dumpNode :: MonadBoolector m => DumpFormat -> FilePath -> Node -> m ()
dumpNode fmt path node = do
  btor <- unBoolectorState `liftM` getBoolectorState
  liftIO $ B.withDumpFile path $ \file -> dumper btor file node
  where dumper = case fmt of
                  DumpBtor -> B.dumpBtorNode
                  _        -> B.dumpSmt2Node

-- | Dump formula to file in BTOR or SMT-LIB v2 format.
dump :: MonadBoolector m => DumpFormat -> FilePath -> m ()
dump fmt path = do
  btor <- unBoolectorState `liftM` getBoolectorState
  liftIO $ B.withDumpFile path (dumper btor)
  where dumper = case fmt of
                  DumpBtor -> B.dumpBtor
                  _        -> B.dumpSmt2

-- | Same as 'dumpNode', but returns string.
-- TODO: this is super slow, we should request feature from boolector.
dumpNodeToString :: MonadBoolector m => DumpFormat -> Node -> m String
dumpNodeToString fmt node = do
  btor <- unBoolectorState `liftM` getBoolectorState
  liftIO $ B.withTempDumpFile (\file -> dumper btor file node)
  where dumper = case fmt of
                  DumpBtor -> B.dumpBtorNode
                  _        -> B.dumpSmt2Node

-- | Same as 'dump', but returns string.
-- TODO: this is super slow, we should request feature from boolector.
dumpToString :: MonadBoolector m => DumpFormat -> m String
dumpToString fmt = do
  btor <- unBoolectorState `liftM` getBoolectorState
  liftIO $ B.withTempDumpFile (dumper btor)
  where dumper = case fmt of
                  DumpBtor -> B.dumpBtor
                  _        -> B.dumpSmt2

--
-- Helpers
--

liftBoolector0 :: MonadBoolector m => (B.Btor -> IO a) -> m a
liftBoolector0 f = do
  s <- getBoolectorState
  liftIO $ f (unBoolectorState s)

liftBoolector1 :: MonadBoolector m => (B.Btor -> a -> IO b) -> a -> m b
liftBoolector1 f x1 = do
  s <- getBoolectorState
  liftIO $ f (unBoolectorState s) x1

liftBoolector2 :: MonadBoolector m => (B.Btor -> a -> b -> IO c) -> a -> b -> m c
liftBoolector2 f x1 x2 = do
  s <- getBoolectorState
  liftIO $ f (unBoolectorState s) x1 x2

liftBoolector3 :: MonadBoolector m => (B.Btor -> a -> b -> c -> IO d) -> a -> b -> c -> m d
liftBoolector3 f x1 x2 x3 = do
  s <- getBoolectorState
  liftIO $ f (unBoolectorState s) x1 x2 x3

--
-- Solver cache
--

-- | Cache sorts and variables.
data BoolectorCache = BoolectorCache {
    sortCache :: SortCache
  , varCache  :: VarCache
  }

-- | Empty boolector cache.
emptyBoolectorCache :: BoolectorCache
emptyBoolectorCache = BoolectorCache emptySortCache Map.empty

-- | Cache sorts.
data SortCache = SortCache {
    scBool   :: Maybe Sort                -- ^ Bool sort
  , scBitVec :: IntMap Sort               -- ^ BitVector sorts
  , scFun    :: Map (Sort, [Sort]) Sort   -- ^ Function sorts
  , scArray  :: Map (Sort, Sort) Sort     -- ^ Array sorts
  }

-- | Empty sort cache.
emptySortCache :: SortCache
emptySortCache = SortCache Nothing IntMap.empty Map.empty Map.empty

-- | Get the sort cache from the underlying state.
getSortCache :: MonadBoolector m => m SortCache
getSortCache = (sortCache . unBoolectorCache) `liftM` getBoolectorState

-- | Set the sort cache into the underlying state.
setSortCache :: MonadBoolector m => SortCache -> m ()
setSortCache sc = do
  s0 <- getBoolectorState
  putBoolectorState $ s0 { unBoolectorCache = (unBoolectorCache s0) { sortCache = sc } }

-- | Variable and uninterpreted function cache.
type VarCache = Map (String, Sort) Node

-- | Get the variable cache from the underlying state.
getVarCache :: MonadBoolector m => m VarCache
getVarCache = (varCache . unBoolectorCache) `liftM` getBoolectorState

-- | Set the variable cache from into underlying state.
setVarCache :: MonadBoolector m => VarCache -> m ()
setVarCache vc = do
  s0 <- getBoolectorState
  putBoolectorState $ s0 { unBoolectorCache = (unBoolectorCache s0) { varCache = vc } }

-- | Create a new named node given a constructor or return it from variable
-- cache. The name must be unique.
createNamedNode :: MonadBoolector m
                => (B.Btor -> Sort -> String -> IO Node)
                -> Sort -> String -> m Node
createNamedNode ctor sort name = do
  vc <- getVarCache
  case Map.lookup (name, sort) vc of
    Just srt -> return srt
    _ -> do node <- liftBoolector2 ctor sort name
            setVarCache $ Map.insert (name, sort) node vc
            return node
