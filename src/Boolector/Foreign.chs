{- |

   This module exports a subset of the low-level C Boolector API to Haskell
   code.  In general, you don't want to use this module and should use the
   'Boolector' module instead.

-}
{-# LANGUAGE StandaloneDeriving #-}
module Boolector.Foreign where

import Foreign
import Foreign.C

import Control.Monad

{#context lib = "boolector" prefix = "boolector_" #}

#include "boolector.h"


--
-- Types
--

-- | Status.
{# enum define Status {
  BOOLECTOR_UNKNOWN as Unknown,
  BOOLECTOR_SAT as Sat,
  BOOLECTOR_UNSAT as Unsat
  } deriving (Eq, Ord, Show ) #}

-- | Boolector instances.
{#pointer *Btor as Btor foreign finalizer delete newtype #}
deriving instance Eq Btor
deriving instance Ord Btor

-- | AST node.
{#pointer *BoolectorNode as Node foreign newtype #}
deriving instance Eq Node
deriving instance Ord Node

-- | Sort.
{#pointer *BoolectorAnonymous as Sort foreign newtype #}
deriving instance Eq Sort
deriving instance Ord Sort

-- | Solver option.
-- See <https://github.com/Boolector/boolector/blob/47f94b39fb6e099195da043ddaf8d82e4b2aebc9/src/btortypes.h#L37>
{# enum define Option {
BTOR_OPT_MODEL_GEN                     as OPT_MODEL_GEN,
BTOR_OPT_INCREMENTAL                   as OPT_INCREMENTAL,
BTOR_OPT_INCREMENTAL_SMT1              as OPT_INCREMENTAL_SMT1,
BTOR_OPT_INPUT_FORMAT                  as OPT_INPUT_FORMAT,
BTOR_OPT_OUTPUT_NUMBER_FORMAT          as OPT_OUTPUT_NUMBER_FORMAT,
BTOR_OPT_OUTPUT_FORMAT                 as OPT_OUTPUT_FORMAT,
BTOR_OPT_ENGINE                        as OPT_ENGINE,
BTOR_OPT_SAT_ENGINE                    as OPT_SAT_ENGINE,
BTOR_OPT_AUTO_CLEANUP                  as OPT_AUTO_CLEANUP,
BTOR_OPT_PRETTY_PRINT                  as OPT_PRETTY_PRINT,
BTOR_OPT_EXIT_CODES                    as OPT_EXIT_CODES,
BTOR_OPT_SEED                          as OPT_SEED,
BTOR_OPT_VERBOSITY                     as OPT_VERBOSITY,
BTOR_OPT_LOGLEVEL                      as OPT_LOGLEVEL,
BTOR_OPT_REWRITE_LEVEL                 as OPT_REWRITE_LEVEL,
BTOR_OPT_SKELETON_PREPROC              as OPT_SKELETON_PREPROC,
BTOR_OPT_ACKERMANN                     as OPT_ACKERMANN,
BTOR_OPT_BETA_REDUCE_ALL               as OPT_BETA_REDUCE_ALL,
BTOR_OPT_ELIMINATE_SLICES              as OPT_ELIMINATE_SLICES,
BTOR_OPT_VAR_SUBST                     as OPT_VAR_SUBST,
BTOR_OPT_UCOPT                         as OPT_UCOPT,
BTOR_OPT_MERGE_LAMBDAS                 as OPT_MERGE_LAMBDAS,
BTOR_OPT_EXTRACT_LAMBDAS               as OPT_EXTRACT_LAMBDAS,
BTOR_OPT_NORMALIZE                     as OPT_NORMALIZE,
BTOR_OPT_NORMALIZE_ADD                 as OPT_NORMALIZE_ADD,
BTOR_OPT_FUN_PREPROP                   as OPT_FUN_PREPROP,
BTOR_OPT_FUN_PRESLS                    as OPT_FUN_PRESLS,
BTOR_OPT_FUN_DUAL_PROP                 as OPT_FUN_DUAL_PROP,
BTOR_OPT_FUN_DUAL_PROP_QSORT           as OPT_FUN_DUAL_PROP_QSORT,
BTOR_OPT_FUN_JUST                      as OPT_FUN_JUST,
BTOR_OPT_FUN_JUST_HEURISTIC            as OPT_FUN_JUST_HEURISTIC,
BTOR_OPT_FUN_LAZY_SYNTHESIZE           as OPT_FUN_LAZY_SYNTHESIZE,
BTOR_OPT_FUN_EAGER_LEMMAS              as OPT_FUN_EAGER_LEMMAS,
BTOR_OPT_FUN_STORE_LAMBDAS             as OPT_FUN_STORE_LAMBDAS,
BTOR_OPT_SLS_NFLIPS                    as OPT_SLS_NFLIPS,
BTOR_OPT_SLS_STRATEGY                  as OPT_SLS_STRATEGY,
BTOR_OPT_SLS_JUST                      as OPT_SLS_JUST,
BTOR_OPT_SLS_MOVE_GW                   as OPT_SLS_MOVE_GW,
BTOR_OPT_SLS_MOVE_RANGE                as OPT_SLS_MOVE_RANGE,
BTOR_OPT_SLS_MOVE_SEGMENT              as OPT_SLS_MOVE_SEGMENT,
BTOR_OPT_SLS_MOVE_RAND_WALK            as OPT_SLS_MOVE_RAND_WALK,
BTOR_OPT_SLS_PROB_MOVE_RAND_WALK       as OPT_SLS_PROB_MOVE_RAND_WALK,
BTOR_OPT_SLS_MOVE_RAND_ALL             as OPT_SLS_MOVE_RAND_ALL,
BTOR_OPT_SLS_MOVE_RAND_RANGE           as OPT_SLS_MOVE_RAND_RANGE,
BTOR_OPT_SLS_MOVE_PROP                 as OPT_SLS_MOVE_PROP,
BTOR_OPT_SLS_MOVE_PROP_N_PROP          as OPT_SLS_MOVE_PROP_N_PROP,
BTOR_OPT_SLS_MOVE_PROP_N_SLS           as OPT_SLS_MOVE_PROP_N_SLS,
BTOR_OPT_SLS_MOVE_PROP_FORCE_RW        as OPT_SLS_MOVE_PROP_FORCE_RW,
BTOR_OPT_SLS_MOVE_INC_MOVE_TEST        as OPT_SLS_MOVE_INC_MOVE_TEST,
BTOR_OPT_SLS_USE_RESTARTS              as OPT_SLS_USE_RESTARTS,
BTOR_OPT_SLS_USE_BANDIT                as OPT_SLS_USE_BANDIT,
BTOR_OPT_PROP_NPROPS                   as OPT_PROP_NPROPS,
BTOR_OPT_PROP_USE_RESTARTS             as OPT_PROP_USE_RESTARTS,
BTOR_OPT_PROP_USE_BANDIT               as OPT_PROP_USE_BANDIT,
BTOR_OPT_PROP_PATH_SEL                 as OPT_PROP_PATH_SEL,
BTOR_OPT_PROP_PROB_USE_INV_VALUE       as OPT_PROP_PROB_USE_INV_VALUE,
BTOR_OPT_PROP_PROB_FLIP_COND           as OPT_PROP_PROB_FLIP_COND,
BTOR_OPT_PROP_PROB_FLIP_COND_CONST     as OPT_PROP_PROB_FLIP_COND_CONST,
BTOR_OPT_PROP_FLIP_COND_CONST_DELTA    as OPT_PROP_FLIP_COND_CONST_DELTA,
BTOR_OPT_PROP_FLIP_COND_CONST_NPATHSEL as OPT_PROP_FLIP_COND_CONST_NPATHSEL,
BTOR_OPT_PROP_PROB_SLICE_KEEP_DC       as OPT_PROP_PROB_SLICE_KEEP_DC,
BTOR_OPT_PROP_PROB_CONC_FLIP           as OPT_PROP_PROB_CONC_FLIP,
BTOR_OPT_PROP_PROB_SLICE_FLIP          as OPT_PROP_PROB_SLICE_FLIP,
BTOR_OPT_PROP_PROB_EQ_FLIP             as OPT_PROP_PROB_EQ_FLIP,
BTOR_OPT_PROP_PROB_AND_FLIP            as OPT_PROP_PROB_AND_FLIP,
BTOR_OPT_PROP_NO_MOVE_ON_CONFLICT      as OPT_PROP_NO_MOVE_ON_CONFLICT,
BTOR_OPT_AIGPROP_USE_RESTARTS          as OPT_AIGPROP_USE_RESTARTS,
BTOR_OPT_AIGPROP_USE_BANDIT            as OPT_AIGPROP_USE_BANDIT,
BTOR_OPT_QUANT_SYNTH                   as OPT_QUANT_SYNTH,
BTOR_OPT_QUANT_DUAL_SOLVER             as OPT_QUANT_DUAL_SOLVER,
BTOR_OPT_QUANT_SYNTH_LIMIT             as OPT_QUANT_SYNTH_LIMIT,
BTOR_OPT_QUANT_SYNTH_ITE_COMPLETE      as OPT_QUANT_SYNTH_ITE_COMPLETE,
BTOR_OPT_QUANT_FIXSYNTH                as OPT_QUANT_FIXSYNTH,
BTOR_OPT_QUANT_SYNTH_QI                as OPT_QUANT_SYNTH_QI,
BTOR_OPT_QUANT_DER                     as OPT_QUANT_DER,
BTOR_OPT_QUANT_CER                     as OPT_QUANT_CER,
BTOR_OPT_QUANT_MINISCOPE               as OPT_QUANT_MINISCOPE,
BTOR_OPT_DEFAULT_TO_CADICAL            as OPT_DEFAULT_TO_CADICAL,
BTOR_OPT_SORT_EXP                      as OPT_SORT_EXP,
BTOR_OPT_SORT_AIG                      as OPT_SORT_AIG,
BTOR_OPT_SORT_AIGVEC                   as OPT_SORT_AIGVEC,
BTOR_OPT_AUTO_CLEANUP_INTERNAL         as OPT_AUTO_CLEANUP_INTERNAL,
BTOR_OPT_SIMPLIFY_CONSTRAINTS          as OPT_SIMPLIFY_CONSTRAINTS,
BTOR_OPT_CHK_FAILED_ASSUMPTIONS        as OPT_CHK_FAILED_ASSUMPTIONS,
BTOR_OPT_CHK_MODEL                     as OPT_CHK_MODEL,
BTOR_OPT_CHK_UNCONSTRAINED             as OPT_CHK_UNCONSTRAINED,
BTOR_OPT_PARSE_INTERACTIVE             as OPT_PARSE_INTERACTIVE,
BTOR_OPT_SAT_ENGINE_LGL_FORK           as OPT_SAT_ENGINE_LGL_FORK,
BTOR_OPT_INCREMENTAL_RW                as OPT_INCREMENTAL_RW,
BTOR_OPT_DECLSORT_BV_WIDTH             as OPT_DECLSORT_BV_WIDTH,
BTOR_OPT_NUM_OPTS                      as OPT_NUM_OPTS
} deriving (Eq, Ord, Show ) #}


--
--  Solver-level interface
--

-- | Create a new instance of Boolector.
{#fun new as ^ { } -> `Btor'  #}

-- | Push new context levels.
{#fun push as ^ { `Btor', `CUInt' } -> `()' #}

-- | Pop context levels.
{#fun pop as ^ { `Btor', `CUInt' } -> `()' #}

-- | Set a termination callback.
setTerm :: Btor -> (Ptr () -> IO Int) -> IO ()
setTerm b callback = do
  cb <- makeWrapper callback
  withBtor b $ \ b' -> setTerm'_ b' cb nullPtr

foreign import ccall "wrapper"
  makeWrapper :: (Ptr () -> IO Int) -> IO (FunPtr (Ptr () -> IO Int))

foreign import ccall "boolector_set_term"
  setTerm'_ :: Ptr Btor -> (FunPtr (Ptr () -> IO Int)) -> Ptr () -> IO ()

--
-- Options
--

-- | Set the SAT solver to use.
--
-- Currently supported: @Lingeling@, @PicoSAT@, and @MiniSAT@.
-- Returns non-zero value if setting the SAT solver was successful.
{#fun set_sat_solver as ^ { `Btor', `String' } -> `()' #}

-- | Set option. See btortypes.h
{#fun set_opt as ^ { `Btor', `Option', `CUInt' } -> `()' #}

-- | Get the current value of an option.
{#fun get_opt as ^ { `Btor', `Option' } -> `CUInt' #}

-- | Check if Boolector has a given option.
{#fun has_opt as ^ { `Btor', `Option' } -> `Bool' #}

--
-- Solving
--

-- | Add a constraint.
{#fun assert as ^ { `Btor' , `Node' } -> `()' #}

-- | Add an assumption.
{#fun assume as ^ { `Btor' , `Node' } -> `()' #}

-- | Determine if assumption @node@ is a failed assumption.
--
-- Failed assumptions are those assumptions, that force an input formula
-- to become unsatisfiable.
{#fun failed as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Add all assumptions as assertions.
{#fun fixate_assumptions as ^ { `Btor' } -> `()' #}

-- | Resets all added assumptions.
{#fun reset_assumptions as ^ { `Btor' } -> `()' #}

-- | Solve an input formula.
--
-- An input formula is defined by constraints added via 'assert'.
-- You can guide the search for a solution to an input formula by making
-- assumptions via 'assume'.
{#fun sat as ^ { `Btor' } -> `Status' #}

-- | Solve an input formula and limit the search by the number of lemmas
-- generated and the number of conflicts encountered by the underlying
-- SAT solver.
--
-- An input formula is defined by constraints added via 'assert'.
-- You can guide the search for a solution to an input formula by making
-- assumptions via 'assume'.
--
-- Returns 'Sat' if the input formula is satisfiable (under possibly given
-- assumptions), 'Usat' if the instance is unsatisfiable, and 'Unknown' if the
-- instance could not be solved within given limits.
{#fun limited_sat as ^ { `Btor' , `Int', `Int' } -> `Status' #}

-- | Simplify current input formula.
{#fun simplify as ^ { `Btor' } -> `Status' #}

--
-- Expressions
--

-- | Copy expression (increments reference counter).
{#fun copy as ^ { `Btor' , `Node' } -> `Node' #}

-- | Release expression (decrements reference counter).
{#fun release as ^ { `Btor' , `Node' } -> `()' #}

-- | Release all expressions and sorts.
{#fun release_all as ^ { `Btor' } -> `()' #}

-- | Create bit vector constant representing the bit vector @bits@.
{#fun const as ^ { `Btor' , `String' } -> `Node' #}

-- | Create bit vector constant representing the decimal number @str@.
{#fun constd as ^ { `Btor' , `Sort', `String' } -> `Node' #}

-- | Create bit vector constant representing the hexadecimal number @str@.
{#fun consth as ^ { `Btor' , `Sort', `String' } -> `Node' #}

-- | Create constant true. This is represented by the bit vector constant one
-- with bit width one.
{#fun true as ^ { `Btor'  } -> `Node' #}

-- | Create bit vector constant zero with bit width one.
{#fun false as ^ { `Btor' } -> `Node' #}

-- | Create bit vector constant zero of sort @sort@.
{#fun zero as ^ { `Btor', `Sort' } -> `Node' #}

-- | Create bit vector constant one of sort @sort@.
{#fun one as ^ { `Btor', `Sort' } -> `Node' #}

-- | Create bit vector constant of sort @sort@, where each bit is set to one.
{#fun ones as ^ { `Btor', `Sort' } -> `Node' #}

-- |  Create bit vector constant representing the unsigned integer @u@ of
-- sort @sort@.
--
-- The constant is obtained by either truncating bits or by
-- unsigned extension (padding with zeroes).
{#fun unsigned_int as ^ { `Btor', `CUInt', `Sort' } -> `Node' #}

-- | Create bit vector constant representing the signed integer @i@ of sort
-- @sort@.
--
-- The constant is obtained by either truncating bits or by
-- signed extension (padding with ones).
{#fun int as ^ { `Btor', `CInt', `Sort' } -> `Node' #}

-- | Create a bit vector variable of sort @sort@.
--
-- The name must be unique.
{#fun var as ^ { `Btor' , `Sort', `String' } -> `Node' #}

-- | Create the one's complement of bit vector @node@.
{#fun not as ^ { `Btor' , `Node'} -> `Node' #}

-- | Create the two's complement of bit vector @node@.
{#fun neg as ^ { `Btor' , `Node'} -> `Node' #}

-- | Create *or* reduction of node @node@.
--
-- All bits of node @node@ are combined by a Boolean *or*.
{#fun redor as ^ { `Btor' , `Node'} -> `Node' #}

-- | Create *xor* reduction of node @node@.
--
-- All bits of @node@ are combined by a Boolean *xor*.
{#fun redxor as ^ { `Btor' , `Node'} -> `Node' #}

-- | Create *and* reduction of node @node@.
--
-- All bits of @node@ are combined by a Boolean *and*.
{#fun redand as ^ { `Btor' , `Node'} -> `Node' #}

-- | Create a bit vector slice of @node@ from index @upper@ to index @lower@.
{#fun slice as ^ { `Btor' , `Node', `CUInt', `CUInt'} -> `Node' #}

-- | Create unsigned extension.
--
-- The bit vector @node@ is padded with @width@ * zeroes.
{#fun uext as ^ { `Btor' , `Node', `CUInt'} -> `Node' #}

-- | Create signed extension.
--
-- The bit vector @node@ is padded with @width@ bits where the value
-- depends on the value of the most significant bit of node @n@.
{#fun sext as ^ { `Btor' , `Node', `CUInt'} -> `Node' #}

-- | Create the concatenation of two bit vectors.
{#fun concat as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create @n@ concatenations of a given node @node@.
{#fun repeat as ^ { `Btor' , `Node', `CUInt'} -> `Node' #}

--
-- Implications.
--

-- | Create boolean implication.
{#fun implies as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create Boolean equivalence.
{#fun iff as ^ { `Btor' , `Node', `Node'} -> `Node' #}

--
-- Equality.
--

-- | Create bit vector or array equality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
{#fun eq as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create bit vector or array inequality.
--
-- Both operands are either bit vectors with the same bit width or arrays
-- of the same type.
{#fun ne as ^ { `Btor' , `Node', `Node'} -> `Node' #}

--
-- Conditionals.
--

-- | Create an if-then-else.
--
-- If condition @n_cond@ is true, then @n_then@ is returned, else @n_else@
-- is returned.
-- Nodes @n_then@ and @n_else@ must be either both arrays or both bit vectors.
{#fun cond as ^ { `Btor' , `Node', `Node', `Node'} -> `Node' #}

--
-- Bit-wise operations.
--

-- | Create a bit vector *xor*.
{#fun xor as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a bit vector *xnor*.
{#fun xnor as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a bit vector *and*.
{#fun and as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a bit vector *nand*.
{#fun nand as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a bit vector *or*.
{#fun or as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a bit vector *nor*.
{#fun nor as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a logical shift left.
--
-- Given node @n1@, the value it represents is the number of zeroes shifted
-- into node @n0@ from the right.
{#fun sll as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a logical shift right.
--
-- Given node @n1@, the value it represents is the number of zeroes shifted
-- into node @n0@ from the left.
{#fun srl as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an arithmetic shift right.
--
-- Analogously to 'srl', but whether zeroes or ones are shifted in depends on
-- the most significant bit of @n0@.
{#fun sra as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a rotate left.
--
-- Given bit vector node @n1@, the value it represents is the number of bits
-- by which node @n0@ is rotated to the left.
{#fun rol as ^ { `Btor' , `Node', `Node'} -> `Node' #}
-- | Create a rotate right.
--
-- Given bit vector node @n1@, the value it represents is the number of bits by
-- which node @n0@ is rotated to the right.
{#fun ror as ^ { `Btor' , `Node', `Node'} -> `Node' #}

--
-- Arithmetic operations.
--

-- | Create bit vector addition.
{#fun add as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create bit vector expression that increments bit vector @node@ by one.
{#fun inc as ^ { `Btor' , `Node' } -> `Node' #}

-- | Create an unsigned bit vector addition overflow detection.
{#fun uaddo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed bit vector addition overflow detection.
{#fun saddo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a bit vector subtraction.
{#fun sub as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an unsigned bit vector subtraction overflow detection.
{#fun usubo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed bit vector subtraction overflow detection.
{#fun ssubo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create bit vector expression that decrements bit vector @node@ by one.
{#fun dec as ^ { `Btor' , `Node' } -> `Node' #}

-- | Create a bitvector multiplication.
{#fun mul as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an unsigned bit vector multiplication overflow detection.
{#fun umulo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create signed multiplication overflow detection.
{#fun smulo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create unsigned division.
{#fun udiv as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create signed division.
{#fun sdiv as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed bit vector division overflow detection.
{#fun sdivo as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an unsigned remainder.
{#fun urem as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed remainder.
{#fun srem as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a, signed remainder where its sign matches the sign of the divisor.
{#fun smod as ^ { `Btor' , `Node', `Node'} -> `Node' #}

--
-- Comparison operations.
--

-- | Create an unsigned less than.
{#fun ult as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed less than.
{#fun slt as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an unsigned less than or equal.
{#fun ulte as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed less than or equal.
{#fun slte as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an unsigned greater than.
{#fun ugt as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed greater than.
{#fun sgt as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create an unsigned greater than or equal.
{#fun ugte as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a signed greater than or equal.
{#fun sgte as ^ { `Btor' , `Node', `Node'} -> `Node' #}

--
-- Array operations
--

-- | Create a one-dimensional bit vector array with sort @sort@.
--
-- The name must be unique.
{#fun array as ^ { `Btor' , `Sort', `String' } -> `Node' #}

-- | Create a read on array @n_array@ at position @n_index@.
{#fun read as ^ { `Btor' , `Node', `Node'} -> `Node' #}

-- | Create a write on array @n_array@ at position @n_index@ with value
-- @n_value@.
--
-- The array is updated at exactly one position, all other elements remain
-- unchanged. The bit width of @n_index@ must be the same as the bit width of
-- the indices of @n_array@. The bit width of @n_value@ must be the same as
-- the bit width of the elements of @n_array@.
{#fun write as ^ { `Btor' , `Node', `Node', `Node'} -> `Node' #}

--
-- Functions
--

-- | Create an uninterpreted function with sort @sort@.
--
-- The name must be unique.
{#fun uf as ^ { `Btor' , `Sort', `String' } -> `Node' #}


-- | Create function parameter of sort @sort@.
--
-- This kind of node is used to create parameterized expressions, which are
-- used to create functions. Once a parameter is bound to a function, it
-- cannot be re-used in other functions.
{#fun param as ^ { `Btor' , `Sort', `String'} -> `Node' #}

-- | Create a function with body @node@ parameterized over parameters
-- @param_nodes@.
--
-- This kind of node is similar to macros in the SMT-LIB standard 2.0.
-- Note that as soon as a parameter is bound to a function, it can not be
-- reused in other functions.
-- Call a function via 'apply'.
fun :: Btor -> [Node] -> Node -> IO Node
fun hbtor hargs hret = withBtor hbtor $ \cbotr ->
  withNodes hargs $ \cargs ->
    withArrayLen cargs $ \len cargsPtr ->
      withNode hret $ \cret -> do
        cptr <- fun'_ cbotr cargsPtr (fromIntegral len) cret
        Node `liftM` newForeignPtr_ cptr

foreign import ccall "boolector_fun"
  fun'_ :: Ptr Btor -> Ptr (Ptr Node) -> CUInt -> Ptr Node -> IO (Ptr Node)

-- | Create a function application on function @n_fun@ with arguments
-- @arg_nodes@.
apply :: Btor -> [Node] -> Node -> IO Node
apply hbtor hargs hfun = withBtor hbtor $ \cbotr ->
  withNodes hargs $ \cargs ->
    withArrayLen cargs $ \len cargsPtr ->
      withNode hfun $ \cfun -> do
        cptr <- apply'_ cbotr cargsPtr (fromIntegral len) cfun
        Node `liftM` newForeignPtr_ cptr

foreign import ccall "boolector_apply"
  apply'_ :: Ptr Btor -> Ptr (Ptr Node) -> CUInt -> Ptr Node -> IO (Ptr Node)

-- | Create a universally quantified term.
forall :: Btor -> [Node] -> Node -> IO Node
forall hbtor hparams hbody = withBtor hbtor $ \cbotr ->
  withNodes hparams $ \cparams ->
    withArrayLen cparams $ \len cparamsPtr ->
      withNode hbody $ \cbody -> do
        cptr <- forall'_ cbotr cparamsPtr (fromIntegral len) cbody
        Node `liftM` newForeignPtr_ cptr

foreign import ccall "boolector_forall"
  forall'_ :: Ptr Btor -> Ptr (Ptr Node) -> CUInt -> Ptr Node -> IO (Ptr Node)

-- | Create an existentially quantifed term.
exists :: Btor -> [Node] -> Node -> IO Node
exists hbtor hparams hbody = withBtor hbtor $ \cbotr ->
  withNodes hparams $ \cparams ->
    withArrayLen cparams $ \len cparamsPtr ->
      withNode hbody $ \cbody -> do
        cptr <- exists'_ cbotr cparamsPtr (fromIntegral len) cbody
        Node `liftM` newForeignPtr_ cptr

foreign import ccall "boolector_exists"
  exists'_ :: Ptr Btor -> Ptr (Ptr Node) -> CUInt -> Ptr Node -> IO (Ptr Node)

-- | Helper function for executing list of Nodes.
withNodes :: [Node] -> ([Ptr Node] -> IO a) -> IO a
withNodes [] f = f []
withNodes (hx:hxs) f = withNode hx $ \cx -> withNodes hxs $ \cxs -> f (cx:cxs)


--
-- Accessors
--

-- | Get the sort of given node.
{#fun get_sort as ^ { `Btor' , `Node' } -> `Sort' #}

-- | Get the domain sort of given function node node.
{#fun fun_get_domain_sort as ^ { `Btor' , `Node' } -> `Sort' #}

-- | Get the codomain sort of given function node node.
{#fun fun_get_codomain_sort as ^ { `Btor' , `Node' } -> `Sort' #}

-- | Get the arity of function node.
{#fun get_fun_arity as ^ { `Btor' , `Node' } -> `CUInt' #}

-- | Get the symbol of an expression.
{#fun get_symbol as ^ { `Btor' , `Node' } -> `String' #}

-- | Set the symbol of an expression.
{#fun set_symbol as ^ { `Btor' , `Node', `String' } -> `()' #}

-- | Get the bit width of an expression.
--
-- If the expression is an array, it returns the bit width of the array
-- elements.
-- If the expression is a function, it returns the bit width of the function's
-- return value.
{#fun get_width as ^ { `Btor' , `Node' } -> `CUInt' #}

-- | Get the bit width of indices of @n_array@.
{#fun get_index_width as ^ { `Btor' , `Node' } -> `CUInt' #}

-- | Determine if given node is a constant node.
{#fun is_const as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given node is a bit vector variable.
{#fun is_var as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given node is an array node.
{#fun is_array as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given node is an array node.
{#fun is_array_var as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given node is a parameter node.
{#fun is_param as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given parameter node is bound by a function.
{#fun is_bound_param as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given node is an uninterpreted function node.
{#fun is_uf as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Determine if given node is a function node.
{#fun is_fun as ^ { `Btor' , `Node' } -> `Bool' #}

-- | Check if sorts of given arguments matches the function signature.
-- Returns 'Nothing' if all sorts are correct; otherwise it returns the
-- position of the incorrect argument.
funSortCheck :: Btor -> [Node] -> Node -> IO (Maybe Int)
funSortCheck hbtor hparams hfun = withBtor hbtor $ \cbotr ->
  withNodes hparams $ \cparams ->
    withArrayLen cparams $ \len cparamsPtr ->
      withNode hfun $ \cfun -> do
       rt <- funSortCheck'_ cbotr cparamsPtr (fromIntegral len) cfun
       return $ if rt == -1
                  then Nothing
                  else Just $ fromIntegral rt

foreign import ccall "boolector_fun_sort_check"
  funSortCheck'_ :: Ptr Btor -> Ptr (Ptr Node) -> CUInt -> Ptr Node -> IO CInt

--
-- Models.
--

-- | Generate an assignment string for bit vector expression if
-- 'sat' has returned 'Sat' and model generation has been enabled.
--
-- The expression can be an arbitrary bit vector expression which
-- occurs in an assertion or current assumption. The assignment string has to
-- be freed by 'free_bv_assignment'.
{#fun bv_assignment as ^ { `Btor' , `Node' } -> `String' #}

-- | Free an assignment string for bit vectors.  TODO: we should change
-- bv_assignment to return a ModelString and  use free to actually free the
-- assignments. We're very leaky right now.
{#fun free_bv_assignment as ^ { `Btor' , `String' } -> `()' #}

--
-- Sorts.
--

-- | Create Boolean sort.
{#fun bool_sort as ^ { `Btor'} -> `Sort' #}

-- | Create bit vector sort of bit width @width@.
{#fun bitvec_sort as ^ { `Btor' , `CUInt' } -> `Sort' #}

-- | Create function sort.
funSort :: Btor -> [Sort] -> Sort -> IO Sort
funSort hbtor hargs hret = withBtor hbtor $ \cbotr ->
  withSorts hargs $ \cargs ->
    withArrayLen cargs $ \len cargsPtr ->
      withSort hret $ \cret -> do
        cptr <- funSort'_ cbotr cargsPtr (fromIntegral len) cret
        Sort `liftM` newForeignPtr_ cptr

foreign import ccall "boolector_fun_sort"
  funSort'_ :: Ptr Btor -> Ptr (Ptr Sort) -> CUInt -> Ptr Sort -> IO (Ptr Sort)

-- | Helper function for executing list of Sorts.
withSorts :: [Sort] -> ([Ptr Sort] -> IO a) -> IO a
withSorts [] f = f []
withSorts (hx:hxs) f = withSort hx $ \cx -> withSorts hxs $ \cxs -> f (cx:cxs)

-- | Create array sort.
{#fun array_sort as ^ { `Btor' , `Sort', `Sort' } -> `Sort' #}

-- | Release sort (decrements reference counter).
{#fun release_sort as ^ { `Btor' , `Sort' } -> `()' #}

-- | Determine if @n0@ and @n1@ have the same sort or not.
{#fun is_equal_sort as ^ { `Btor' , `Node', `Node' } -> `Bool' #}

-- | Determine if @sort@ is an array sort.
{#fun is_array_sort as ^ { `Btor' , `Sort' } -> `Bool' #}

-- | Determine if @sort@ is a bit-vector sort.
{#fun is_bitvec_sort as ^ { `Btor' , `Sort' } -> `Bool' #}

-- | Determine if @sort@ is a function sort.
{#fun is_fun_sort as ^ { `Btor' , `Sort' } -> `Bool' #}

--
-- Dumping
--

{#pointer *FILE as File foreign finalizer fclose newtype#}

-- | libc's fopen
{#fun fopen as ^ {`String', `String'} -> `File' #}

-- | Recursively dump @node@ to file in BTOR_ format.
{#fun dump_btor_node as ^ { `Btor' , `File', `Node' } -> `()' #}

-- | Dump formula to file in BTOR_ format.
{#fun dump_btor as ^ { `Btor' , `File' } -> `()' #}

-- | Recursively dump @node@ to file in SMT-LIB v2 format.
{#fun dump_smt2_node as ^ { `Btor' , `File', `Node' } -> `()' #}

-- | Dumps formula to file in SMT-LIB v2 format.
{#fun dump_smt2 as ^ { `Btor' , `File' } -> `()' #}
