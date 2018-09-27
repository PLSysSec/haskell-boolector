{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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
                 , createDefaultSorts
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
                 , inc
                 , sub
                 , dec
                 , mul
                 , udiv
                 , sdiv
                 , urem
                 , srem
                 , smod
                 -- **** Overflow detection
                 , uaddo
                 , saddo
                 , usubo
                 , ssubo
                 , umulo
                 , smulo
                 , sdivo
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
                 , SortTy, sortTy
                 , boolSort
                 , bitvecSort
                 , funSort
                 , arraySort
                 -- *** Accessors
                 , isEqualSort
                 , isArraySort
                 , isBoolSort
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

import Boolector.Foreign (Option(..), Status(..))
import qualified Boolector.Foreign as B

import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word

import Control.Applicative ((<$>))
import Control.Monad.State.Strict
import Control.Exception hiding (assert)

import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.Clock.System

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
evalBoolector bState act = evalStateT (unBoolector $ createDefaultSorts >> act) bState

-- | Like 'evalBoolector', but take an explicit starting BoolectorState, and
-- return the final BoolectorState
runBoolector :: BoolectorState -> Boolector a -> IO (a, BoolectorState)
runBoolector bState act = runStateT (unBoolector $ createDefaultSorts >> act) bState

-- | Create new Boolector state with optional timeout. By default, we enable
-- support for model generation and incremental solving.
newBoolectorState :: Maybe Integer -> IO BoolectorState
newBoolectorState Nothing = do
  b <- B.new
  B.setOpt b OPT_MODEL_GEN 2
  B.setOpt b OPT_AUTO_CLEANUP 1
  B.setOpt b OPT_INCREMENTAL 1
  return $ BoolectorState b emptyBoolectorCache
newBoolectorState (Just time) = do
  btorState@(BoolectorState b _) <- newBoolectorState Nothing
  t0 <- systemToTAITime `liftM` getSystemTime
  B.setTerm b $ \_ -> do
    t1 <- systemToTAITime `liftM` getSystemTime
    let shouldTerminate = diffAbsoluteTime t1 t0 > secondsToDiffTime time
    return $ if shouldTerminate then 1 else 0
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
assert = liftBoolector1 B.assert . _node

-- | Add an assumption.
assume :: MonadBoolector m => Node -> m ()
assume = liftBoolector1 B.assume . _node

-- | Determine if assumption node is a failed assumption.
failed :: MonadBoolector m => Node -> m Bool
failed = liftBoolector1 B.failed . _node

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

-- | Node data type wrapping the underlying Boolector node with a show string.
data Node = Node { _node     :: B.Node
                 , _showNode :: String } deriving (Eq, Ord)

instance Show Node where
  show = _showNode

-- | Like true and false
bool :: MonadBoolector m => Bool -> m Node
bool True  = true
bool False = false

-- | Create constant true. This is represented by the bit vector constant one
-- with bit width one.
true :: MonadBoolector m => m Node
true = mkNode "true" $ liftBoolector0 B.true

-- | Create bit vector constant zero with bit width one.
false :: MonadBoolector m => m Node
false = mkNode "false" $ liftBoolector0 B.false

-- | Create bit vector constant representing the bit vector @bits@.
const :: MonadBoolector m => String -> m Node
const str = mkNode ("0b" ++ str) $ liftBoolector1 B.const str

-- | Create bit vector constant representing the decimal number @str@.
constd :: MonadBoolector m => Sort -> String -> m Node
constd srt str = mkNode str $ liftBoolector2 B.constd (_sort srt) str

-- | Create bit vector constant representing the hexadecimal number @str@.
consth :: MonadBoolector m => Sort -> String -> m Node
consth srt str = mkNode ("0x" ++ str) $ liftBoolector2 B.consth (_sort srt) str

-- | Create bit vector constant zero of sort @sort@.
zero :: MonadBoolector m => Sort -> m Node
zero = mkNode "zero" . liftBoolector1 B.zero . _sort

-- | Create bit vector constant of sort @sort@, where each bit is set to one.
ones :: MonadBoolector m => Sort -> m Node
ones srt = mkNode onesStr $ liftBoolector1 B.one $ _sort srt
  where onesStr = "0b" ++ replicate nr '1'
        nr = case sortTy srt of
              BoolSort -> 1
              BitVecSort wNr -> fromIntegral wNr
              _ -> error "invalid sort"

-- | Create bit vector constant one of sort @sort@.
one :: MonadBoolector m => Sort -> m Node
one = mkNode "1" . liftBoolector1 B.one . _sort

-- |  Create bit vector constant representing the unsigned integer @u@ of
-- sort @sort@.
--
-- The constant is obtained by either truncating bits or by unsigned extension
-- (padding with zeroes).
unsignedInt :: MonadBoolector m => Integer -> Sort -> m Node
unsignedInt i srt = constd srt (show i)

-- | Create bit vector constant representing the signed integer @i@ of sort
-- @sort@.
--
-- The constant is obtained by either truncating bits or by
-- signed extension (padding with ones).
signedInt :: MonadBoolector m => Integer -> Sort -> m Node
signedInt i srt = constd srt (show i)


-- | Create a bit vector variable of sort @sort@.
var :: MonadBoolector m => Sort -> String -> m Node
var srt str = mkNamedNode "var" B.var srt str

-- | Create the one's complement of bit vector @node@.
not :: MonadBoolector m => Node -> m Node
not n1 = mkNode ["not", show n1] $ liftBoolector1 B.not (_node n1)

-- | Create the two's complement of bit vector @node@.
neg :: MonadBoolector m => Node -> m Node
neg n1 = mkNode ["neg", show n1] $ liftBoolector1 B.neg (_node n1)

-- | Create *or* reduction of node @node@.
--
-- All bits of node @node@ are combined by a Boolean *or*.
redor :: MonadBoolector m => Node -> m Node
redor n1 = mkNode ["redor", show n1] $ liftBoolector1 B.redor (_node n1)

-- | Create *xor* reduction of node @node@.
--
-- All bits of @node@ are combined by a Boolean *xor*.
redxor :: MonadBoolector m => Node -> m Node
redxor n1 = mkNode ["redxor", show n1] $ liftBoolector1 B.redxor (_node n1)

-- | Create *and* reduction of node @node@.
--
-- All bits of @node@ are combined by a Boolean *and*.
redand :: MonadBoolector m => Node -> m Node
redand n = mkNode ["redand", show n] $ liftBoolector1 B.redand (_node n)

-- | Create a bit vector slice of @node@ from index @upper@ to index @lower@.
slice :: MonadBoolector m
      => Node -- ^ Bit vector node.
      -> Word32 -- ^ Upper index which must be greater than or equal to zero, and less than the bit width of @node@.
      -> Word32 -- ^ Lower index which must be greater than or equal to zero, and less than or equal to @upper@.
      -> m Node
slice n u l = do
  -- Create sort if not already in cache
  void $ bitvecSort (fromIntegral $ u - l + 1)
  --
  mkNode ["slice", show n, show u, show l] $
    liftBoolector3 B.slice (_node n) (fromIntegral u) (fromIntegral l)

-- | Create unsigned extension.
--
-- The bit vector @node@ is padded with @width@ * zeroes.
uext :: MonadBoolector m => Node -> Word32 -> m Node
uext n w = do
  -- Create sort if not already in cache
  nw <- getWidth n
  void $ bitvecSort (fromIntegral $ nw + w)
  --
  mkNode ["uext", show n, show w] $
    liftBoolector2 B.uext (_node n) (fromIntegral w)

-- | Create signed extension.
--
-- The bit vector @node@ is padded with @width@ bits where the value
-- depends on the value of the most significant bit of node @n@.
sext :: MonadBoolector m => Node -> Word32 -> m Node
sext n w = do
  -- Create sort if not already in cache
  nw <- getWidth n
  void $ bitvecSort (fromIntegral $ nw + w)
  --
  mkNode ["sext", show n, show w] $
    liftBoolector2 B.sext (_node n) (fromIntegral w)

-- | Create the concatenation of two bit vectors.
concat :: MonadBoolector m => Node -> Node -> m Node
concat n1 n2 = do
  -- Create sort if not already in cache
  nw1 <- getWidth n1
  nw2 <- getWidth n2
  void $ bitvecSort (fromIntegral $ nw1 + nw2)
  --
  mkNode ["concat", show n1, show n2] $
    liftBoolector2 B.concat (_node n1) (_node n2)

-- | Create @n@ concatenations of a given node @node@.
repeat :: MonadBoolector m => Node -> Word32 -> m Node
repeat n w = do
  -- Create sort if not already in cache
  nw <- getWidth n
  void $ bitvecSort (fromIntegral $ nw * w)
  --
  mkNode ["repeat", show n, show w] $
    liftBoolector2 B.repeat (_node n) (fromIntegral w)

-- | Create boolean implication.
implies :: MonadBoolector m => Node -> Node -> m Node
implies n1 n2 = mkNode ["implies", show n1, show n2] $
                liftBoolector2 B.implies (_node n1) (_node n2)

-- | Create Boolean equivalence.
iff :: MonadBoolector m => Node -> Node -> m Node
iff n1 n2 = mkNode ["iff", show n1, show n2] $
            liftBoolector2 B.iff (_node n1) (_node n2)

-- | Create bit vector or array equality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
eq :: MonadBoolector m => Node -> Node -> m Node
eq n1 n2 = mkNode ["eq", show n1, show n2] $
           liftBoolector2 B.eq (_node n1) (_node n2)

-- | Create bit vector or array inequality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
ne :: MonadBoolector m => Node -> Node -> m Node
ne n1 n2 = mkNode ["ne", show n1, show n2] $
           liftBoolector2 B.ne (_node n1) (_node n2)

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
cond n1 n2 n3 = mkNode ["cond", show n1, show n2, show n3] $
                liftBoolector3 B.cond (_node n1) (_node n2) (_node n3)

--
-- Bit-wise operations.
--

-- | Create a bit vector *xor*.
xor :: MonadBoolector m => Node -> Node -> m Node
xor n1 n2 = mkNode ["xor", show n1, show n2] $ liftBoolector2 B.xor (_node n1) (_node n2)

-- | Create a bit vector *xnor*.
xnor :: MonadBoolector m => Node -> Node -> m Node
xnor n1 n2 = mkNode ["xnor", show n1, show n2] $ liftBoolector2 B.xnor (_node n1) (_node n2)

-- | Create a bit vector *and*.
and  :: MonadBoolector m => Node -> Node -> m Node
and n1 n2 = mkNode ["and", show n1, show n2] $ liftBoolector2 B.and (_node n1) (_node n2)

-- | Create a bit vector *nand*.
nand :: MonadBoolector m => Node -> Node -> m Node
nand n1 n2 = mkNode ["nand", show n1, show n2] $ liftBoolector2 B.nand (_node n1) (_node n2)

-- | Create a bit vector *or*.
or :: MonadBoolector m => Node -> Node -> m Node
or n1 n2 = mkNode ["or", show n1, show n2] $ liftBoolector2 B.or (_node n1) (_node n2)

-- | Create a bit vector *nor*.
nor :: MonadBoolector m => Node -> Node -> m Node
nor n1 n2 = mkNode ["nor", show n1, show n2] $ liftBoolector2 B.nor (_node n1) (_node n2)

-- | Create a logical shift left.
--
-- Given node @n1@, the value it represents is the number of zeroes shifted
-- into node @n0@ from the right.
sll :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
sll n1 n2 = mkNode ["sll", show n1, show n2] $ liftBoolector2 B.sll (_node n1) (_node n2)

-- | Create a logical shift right.
--
-- Given node @n1@, the value it represents is the number of zeroes shifted
-- into node @n0@ from the left.
srl :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
srl n1 n2 = mkNode ["srl", show n1, show n2] $ liftBoolector2 B.srl (_node n1) (_node n2)

-- | Create an arithmetic shift right.
--
-- Analogously to 'srl', but whether zeroes or ones are shifted in depends on
-- the most significant bit of @n0@.
sra :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
sra n1 n2 = mkNode ["sra", show n1, show n2] $ liftBoolector2 B.sra (_node n1) (_node n2)

-- | Create a rotate left.
--
-- Given bit vector node @n1@, the value it represents is the number of bits
-- by which node @n0@ is rotated to the left.
rol :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
rol n1 n2 = mkNode ["rol", show n1, show n2] $ liftBoolector2 B.rol (_node n1) (_node n2)

-- | Create a rotate right.
--
-- Given bit vector node @n1@, the value it represents is the number of bits by
-- which node @n0@ is rotated to the right.
ror :: MonadBoolector m
    => Node -- ^ First bit vector operand where the bit width is a power of two and greater than 1.
    -> Node -- ^ Second bit vector operand with bit width log2 of the bit width of @n0@.
    -> m Node
ror n1 n2 = mkNode ["ror", show n1, show n2] $ liftBoolector2 B.ror (_node n1) (_node n2)

--
-- Arithmetic operations.
--

-- | Create bit vector addition.
add :: MonadBoolector m => Node -> Node -> m Node
add n1 n2 = mkNode ["add", show n1, show n2] $ liftBoolector2 B.add (_node n1) (_node n2)

-- | Create bit vector expression that increments bit vector @node@ by one.
inc :: Node ->  Boolector Node
inc n = mkNode ["inc", show n] $ liftBoolector1 B.inc (_node n)

-- | Create a bit vector subtraction.
sub :: MonadBoolector m => Node -> Node -> m Node
sub n1 n2 = mkNode ["sub", show n1, show n2] $ liftBoolector2 B.sub (_node n1) (_node n2)

-- | Create bit vector expression that decrements bit vector @node@ by one.
dec :: MonadBoolector m => Node -> m Node
dec n = mkNode ["dec", show n] $ liftBoolector1 B.dec (_node n)

-- | Create a bitvector multiplication.
mul :: MonadBoolector m => Node -> Node -> m Node
mul n1 n2 = mkNode ["mul", show n1, show n2] $ liftBoolector2 B.mul (_node n1) (_node n2)

-- | Create unsigned division.
udiv :: MonadBoolector m => Node -> Node -> m Node
udiv n1 n2 = mkNode ["udiv", show n1, show n2] $ liftBoolector2 B.udiv (_node n1) (_node n2)

-- | Create signed division.
sdiv :: MonadBoolector m => Node -> Node -> m Node
sdiv n1 n2 = mkNode ["sdiv", show n1, show n2] $ liftBoolector2 B.sdiv (_node n1) (_node n2)

-- | Create an unsigned remainder.
urem :: MonadBoolector m => Node -> Node -> m Node
urem n1 n2 = mkNode ["urem", show n1, show n2] $ liftBoolector2 B.urem (_node n1) (_node n2)

-- | Create a signed remainder.
srem :: MonadBoolector m => Node -> Node -> m Node
srem n1 n2 = mkNode ["srem", show n1, show n2] $ liftBoolector2 B.srem (_node n1) (_node n2)

-- | Create a, signed remainder where its sign matches the sign of the divisor.
smod :: MonadBoolector m => Node -> Node -> m Node
smod n1 n2 = mkNode ["smod", show n1, show n2] $ liftBoolector2 B.smod (_node n1) (_node n2)

--
-- Overflow detection
--

-- | Create an unsigned bit vector subtraction overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
usubo :: MonadBoolector m => Node -> Node -> m Node
usubo n1 n2 = mkNode ["usubo", show n1, show n2] $ liftBoolector2 B.usubo (_node n1) (_node n2)

-- | Create a signed bit vector subtraction overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
ssubo :: MonadBoolector m => Node -> Node -> m Node
ssubo n1 n2 = mkNode ["ssubo", show n1, show n2] $ liftBoolector2 B.ssubo (_node n1) (_node n2)

-- | Create an unsigned bit vector addition overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
uaddo :: MonadBoolector m => Node -> Node -> m Node
uaddo n1 n2 = mkNode ["uaddo", show n1, show n2] $ liftBoolector2 B.uaddo (_node n1) (_node n2)

-- | Create a signed bit vector addition overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
saddo :: MonadBoolector m => Node -> Node -> m Node
saddo n1 n2 = mkNode ["saddo", show n1, show n2] $ liftBoolector2 B.saddo (_node n1) (_node n2)

-- | Create an unsigned bit vector multiplication overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
umulo :: MonadBoolector m => Node -> Node -> m Node
umulo n1 n2 = mkNode ["umulo", show n1, show n2] $ liftBoolector2 B.umulo (_node n1) (_node n2)

-- | Create signed multiplication overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
smulo :: MonadBoolector m => Node -> Node -> m Node
smulo n1 n2 = mkNode ["smulo", show n1, show n2] $ liftBoolector2 B.smulo (_node n1) (_node n2)

-- | Create a signed bit vector division overflow detection.
-- Returns bit vector with bit-width one, which indicates if the operation
-- overflows.
sdivo :: MonadBoolector m => Node -> Node -> m Node
sdivo n1 n2 = mkNode ["sdivo", show n1, show n2] $ liftBoolector2 B.sdivo (_node n1) (_node n2)

--
-- Comparison operations.
--

-- | Create an unsigned less than.
ult :: MonadBoolector m => Node -> Node -> m Node
ult n1 n2 = mkNode ["ult", show n1, show n2] $ liftBoolector2 B.ult (_node n1) (_node n2)

-- | Create a signed less than.
slt :: MonadBoolector m => Node -> Node -> m Node
slt n1 n2 = mkNode ["slt", show n1, show n2] $ liftBoolector2 B.slt (_node n1) (_node n2)

-- | Create an unsigned less than or equal.
ulte :: MonadBoolector m => Node -> Node -> m Node
ulte n1 n2 = mkNode ["ulte", show n1, show n2] $ liftBoolector2 B.ulte (_node n1) (_node n2)

-- | Create a signed less than or equal.
slte :: MonadBoolector m => Node -> Node -> m Node
slte n1 n2 = mkNode ["slte", show n1, show n2] $ liftBoolector2 B.slte (_node n1) (_node n2)

-- | Create an unsigned greater than.
ugt :: MonadBoolector m => Node -> Node -> m Node
ugt n1 n2 = mkNode ["ugt", show n1, show n2] $ liftBoolector2 B.ugt (_node n1) (_node n2)

-- | Create a signed greater than.
sgt :: MonadBoolector m => Node -> Node -> m Node
sgt n1 n2 = mkNode ["sgt", show n1, show n2] $ liftBoolector2 B.sgt (_node n1) (_node n2)

-- | Create an unsigned greater than or equal.
ugte :: MonadBoolector m => Node -> Node -> m Node
ugte n1 n2 = mkNode ["ugte", show n1, show n2] $ liftBoolector2 B.ugte (_node n1) (_node n2)

-- | Create a signed greater than or equal.
sgte :: MonadBoolector m => Node -> Node -> m Node
sgte n1 n2 = mkNode ["sgte", show n1, show n2] $ liftBoolector2 B.sgte (_node n1) (_node n2)

--
-- Array operations
--

-- | Create a one-dimensional bit vector array with sort @sort@.
--
-- The name must be unique.
array :: MonadBoolector m => Sort -> String -> m Node
array srt str = mkNamedNode "array" B.array srt str

-- | Create a read on array @n_array@ at position @n_index@.
read :: MonadBoolector m
     => Node -- ^ Array operand.
     -> Node -- ^ Bit vector index. The bit width of @n_index@ must have the same bit width as the indices of @n_array@.
     -> m Node
read n1 n2 = mkNode ["read", show n1, show n2] $ liftBoolector2 B.read (_node n1) (_node n2)

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
write n1 n2 n3 = mkNode ["write", show n1, show n2, show n3] $ liftBoolector3 B.write (_node n1) (_node n2) (_node n3)

--
-- Functions
--

-- | Create an uninterpreted function with sort @sort@.
--
-- The name must be unique.
uf :: MonadBoolector m => Sort -> String -> m Node
uf srt str = mkNamedNode "uf" B.uf srt str

-- | Create function parameter of sort @sort@.
--
-- This kind of node is used to create parameterized expressions, which are
-- used to create functions. Once a parameter is bound to a function, it
-- cannot be re-used in other functions.
param :: MonadBoolector m => Sort -> String -> m Node
param srt str = mkNode ["param", show srt, str] $ liftBoolector2 B.param (_sort srt) str

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
fun n1 n2 = mkNode ["fun", show n1, show n2] $ liftBoolector2 B.fun (map _node n1) (_node n2)

-- | Create a function application on function @n_fun@ with arguments
-- @arg_nodes@.
apply :: MonadBoolector  m
      => [Node] -- ^ Arguments to be applied.
      -> Node   -- ^ Number of arguments to be applied.
      -> m Node
apply n1 n2 = mkNode ["apply", show n1, show n2] $  liftBoolector2 B.apply (map _node n1) (_node n2)


--
-- Quantified terms
--

-- | Create a universally quantified term.
forall :: MonadBoolector m
       => [Node] -- ^ Quantified variables (create with 'param')
       -> Node   -- ^ Term where variables may occur. (Cannot contain functions.)
       -> m Node
forall n1 n2 = mkNode ["forall", show n1, show n2] $  liftBoolector2 B.forall (map _node n1) (_node n2)

-- | Create an existentially quantifed term.
exists :: MonadBoolector m
       => [Node] -- ^ Quantified variables (create with 'param')
       -> Node   -- ^ Term where variables may occur. (Cannot contain functions.)
       -> m Node
exists n1 n2 = mkNode ["exists", show n1, show n2] $  liftBoolector2 B.exists (map _node n1) (_node n2)

--
-- Accessors
--

-- | Get the sort of given @node@. The result does not have to be released.
getSort :: MonadBoolector m => Node -> m Sort
getSort n = liftBoolector1 B.getSort (_node n) >>= lookupSort

-- | Get the domain sort of given function node @node@.
--
-- The result does not have to be released.
funGetDomainSort :: MonadBoolector m => Node -> m Sort
funGetDomainSort n = liftBoolector1 B.funGetDomainSort (_node n) >>= lookupSort

-- | Get the codomain sort of given function node @node@.
--
-- The result does not have to be released.
funGetCodomainSort :: MonadBoolector m => Node -> m Sort
funGetCodomainSort n = liftBoolector1 B.funGetCodomainSort (_node n) >>= lookupSort

-- | Get the arity of function node.
funGetArity :: MonadBoolector m => Node -> m Word
funGetArity n = fromIntegral `liftM` liftBoolector1 B.getFunArity (_node n)

-- | Get the symbol of an expression.
getSymbol :: MonadBoolector m => Node -> m (Maybe String)
getSymbol = liftBoolector1 B.getSymbol . _node

-- | Set the symbol of an expression.
setSymbol :: MonadBoolector m => Node -> String -> m ()
setSymbol n str = liftBoolector2 B.setSymbol (_node n) str

-- | Get the bit width of an expression.
--
-- If the expression is an array, it returns the bit width of the array
-- elements.
-- If the expression is a function, it returns the bit width of the function's
-- return value.
getWidth :: MonadBoolector m => Node -> m Word32
getWidth n = fromIntegral `liftM` liftBoolector1 B.getWidth (_node n)

-- | Get the bit width of indices of @n_array@.
getIndexWidth :: MonadBoolector m => Node -> m Word32
getIndexWidth n = fromIntegral `liftM` liftBoolector1 B.getIndexWidth (_node n)

-- | Determine if given node is a constant node.
isConst :: MonadBoolector m => Node -> m Bool
isConst = liftBoolector1 B.isConst . _node

-- | Determine if given node is a bit vector variable.
isVar :: MonadBoolector m => Node -> m Bool
isVar = liftBoolector1 B.isVar . _node

-- | Determine if given node is an array node.
isArray :: MonadBoolector m => Node -> m Bool
isArray = liftBoolector1 B.isArray . _node

-- | Determine if given node is an array node.
isArrayVar :: MonadBoolector m => Node -> m Bool
isArrayVar = liftBoolector1 B.isArrayVar . _node

-- | Determine if given node is a parameter node.
isParam :: MonadBoolector m => Node -> m Bool
isParam = liftBoolector1 B.isParam . _node

-- | Determine if given parameter node is bound by a function.
isBoundParam :: MonadBoolector m => Node -> m Bool
isBoundParam = liftBoolector1 B.isBoundParam . _node

-- | Determine if given node is an uninterpreted function node.
isUf :: MonadBoolector m => Node -> m Bool
isUf = liftBoolector1 B.isUf . _node

-- | Determine if given node is a function node.
isFun :: MonadBoolector m => Node -> m Bool
isFun = liftBoolector1 B.isFun . _node


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
bvAssignment = liftBoolector1 B.bvAssignment . _node

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

-- | Type of sorts, used to keep track of sorts without having to go back into C-land.
data SortTy = BoolSort
            | BitVecSort Word
            | FunSort [SortTy] SortTy
            | ArraySort SortTy SortTy
            deriving (Eq, Ord, Show)

-- | Sort wraps the udnerlying Boolector sort with a showable type.
data Sort = Sort { sortTy :: SortTy -- ^ Get sort type
                 , _sort  :: B.Sort
                 } deriving (Eq, Ord)

instance Show Sort where
  show = show . sortTy

-- | Create some default, sane sorts.
createDefaultSorts :: Boolector ()
createDefaultSorts = do
  void $ boolSort
  void $ bitvecSort 1
  void $ bitvecSort 2
  void $ bitvecSort 4
  void $ bitvecSort 8
  void $ bitvecSort 16
  void $ bitvecSort 32
  void $ bitvecSort 64
  void $ bitvecSort 128

-- | Create Boolean sort.
boolSort :: Boolector Sort
boolSort = do
  sc <- getSortCache
  case scBool sc of
    Just srt -> return srt
    _ -> do srt <- Sort BoolSort <$> liftBoolector0 B.boolSort
            setSortCache $ sc { scBool = Just srt }
            return srt

-- | Create bit vector sort of bit width @width@.
bitvecSort :: MonadBoolector m => Word -> m Sort
bitvecSort wnr = do
  sc <- getSortCache
  let bvMap = scBitVec sc
  case IntMap.lookup nr bvMap of
    Just srt -> return srt
    _ -> do srt <- Sort (BitVecSort nr) <$> liftBoolector1 B.bitvecSort nr
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
    _ -> do srt <- Sort (FunSort (map sortTy args) (sortTy ret))
                       <$> liftBoolector2 B.funSort (map _sort args) (_sort ret)
            setSortCache $ sc { scFun = Map.insert (ret, args) srt funMap }
            return srt

-- | Create array sort.
arraySort :: MonadBoolector m => Sort -> Sort -> m Sort
arraySort dom rng = do
  sc <- getSortCache
  let arrMap = scArray sc
  case Map.lookup (dom, rng) arrMap of
    Just srt -> return srt
    _ -> do srt <- Sort (ArraySort (sortTy dom) (sortTy rng))
                      <$> liftBoolector2 B.arraySort (_sort dom) (_sort rng)
            setSortCache $ sc { scArray = Map.insert (dom, rng) srt arrMap }
            return srt

-- | Determine if @n0@ and @n1@ have the same sort or not.
isEqualSort :: MonadBoolector m => Node -> Node -> m Bool
isEqualSort n1 n2 = liftBoolector2 B.isEqualSort (_node n1) (_node n2)

-- | Determine if @sort@ is an array sort.
isArraySort :: Sort -> Bool
isArraySort srt = case sortTy srt of
                    ArraySort _ _ -> True
                    _ -> False

-- | Determine if @sort@ is a bool sort.
isBoolSort :: Sort -> Bool
isBoolSort srt = case sortTy srt of
                    BoolSort -> True
                    _ -> False

-- | Determine if @sort@ is a bit-vector sort.
isBitvecSort :: Sort -> Bool
isBitvecSort srt = case sortTy srt of
                    BitVecSort _ -> True
                    _ -> False

-- | Determine if @sort@ is a function sort.
isFunSort :: Sort -> Bool
isFunSort srt = case sortTy srt of
                    FunSort _ _ -> True
                    _ -> False

-- | Check if sorts of given arguments matches the function signature.
-- Returns 'Nothing' if all sorts are correct; otherwise it returns the
-- position of the incorrect argument.
funSortCheck :: MonadBoolector m => [Node] -> Node -> m (Maybe Int)
funSortCheck n1 n2 = liftBoolector2 B.funSortCheck (map _node n1) (_node n2)


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
  liftIO $ B.withDumpFile path $ \file -> dumper btor file (_node node)
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
  liftIO $ B.withTempDumpFile (\file -> dumper btor file (_node node))
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


--
-- Internal helpers
--

-- | Class used to create nodes from boolector nodes, given a stringification
class Show s => MkNode s where
  mkNode :: MonadBoolector m => s -> m B.Node -> m Node

instance MkNode String where
  mkNode str act = do
    node <- act
    return $ Node node str

instance MkNode [String] where
  mkNode str act = do
    node <- act
    return $ Node node $ "(" ++ unwords str ++ ")"


-- | Create a new named node given a constructor or return it from variable
-- cache. The name must be unique.
mkNamedNode :: MonadBoolector m
            => String                                    -- ^ Kind of node
            -> (B.Btor -> B.Sort -> String -> IO B.Node) -- ^ Underlying constructor
            -> Sort                                      -- ^ Sort of node
            -> String                                    -- ^ Name of node
            -> m Node
mkNamedNode kind ctor sort name = do
  vc <- getVarCache
  case Map.lookup (name, sort) vc of
    Just srt -> return srt
    _ -> do node <- mkNode [kind, name, "::", show sort] $
                      liftBoolector2 ctor (_sort sort) name
            setVarCache $ Map.insert (name, sort) node vc
            return node

-- | Get the high level sort from cache that corresponds to boolector sort
lookupSort :: MonadBoolector m => B.Sort -> m Sort
lookupSort bSort = do
  sc <- getSortCache
  case () of
    _ | Just srt <- lookupBoolSort sc   -> return srt
    _ | Just srt <- lookupBitVecSort sc -> return srt
    _ | Just srt <- lookupFunSort sc    -> return srt
    _ | Just srt <- lookupArraySort sc  -> return srt
    _ -> fail "BUG: should really have the sort in the cache"
  where lookupBoolSort sc = case scBool sc of
                              Just srt | _sort srt == bSort -> Just srt
                              _ -> Nothing
        lookupBitVecSort sc = listToMaybe $ IntMap.elems $
                                IntMap.filter (\s -> _sort s == bSort) $ scBitVec sc
        lookupFunSort sc = listToMaybe $ Map.elems $
                                Map.filter (\s -> _sort s == bSort) $ scFun sc
        lookupArraySort sc = listToMaybe $ Map.elems $
                                Map.filter (\s -> _sort s == bSort) $ scArray sc
