module Boolector.Foreign where

import Foreign
import Foreign.C

import Control.Monad
import Control.Applicative

{#context lib = "boolector" prefix = "boolector_" #}

#include "boolector.h"

{# enum define Status { BOOLECTOR_UNKNOWN as UNKNOWN, BOOLECTOR_SAT as SAT, BOOLECTOR_UNSAT as UNSAT, BOOLECTOR_PARSE_ERROR as PARSE_ERROR }
  deriving (Eq, Ord, Show ) #}

-- {#pointer *Btor as Tor foreign finalizer boolector_delete newtype #}
{#pointer *Btor as Tor foreign newtype #}
    
{#pointer *BoolectorNode as Node newtype #}

-- | functions are as listed in the python API
-- boolector/api/python/btorapi.pxd .
-- some are commented out (for now)


{#fun new as ^ { } -> `Tor'  #}
{#fun clone as ^ { `Tor' } -> `Tor' #}
{#fun delete as ^ { `Tor' } -> `()' #}

{#fun get_refs as ^ { `Tor' } -> `Int' #}
{#fun reset_time as ^ { `Tor' } -> `()' #}
{#fun reset_stats as ^ { `Tor' } -> `()' #}
{#fun print_stats as ^ { `Tor' } -> `()' #}

{#fun set_opt as ^ { `Tor', `String', `Int' } -> `()' #}
{#fun get_opt_val as ^ { `Tor', `String' } -> `Int' #}

{#fun assert as ^ { `Tor' , `Node' } -> `()' #}
{#fun assume as ^ { `Tor' , `Node' } -> `()' #}
{#fun failed as ^ { `Tor' , `Node' } -> `Bool' #}
{#fun sat as ^ { `Tor' } -> `Status' #}
{#fun limited_sat as ^ { `Tor' , `Int', `Int' } -> `Status' #}
{#fun simplify as ^ { `Tor' } -> `Status' #}

{#fun copy as ^ { `Tor' , `Node' } -> `Node' #}
{#fun release as ^ { `Tor' , `Node' } -> `()' #}

{#fun const as ^ { `Tor' , `String' } -> `Node' #}
{#fun false as ^ { `Tor' } -> `Node' #}
{#fun zero as ^ { `Tor', `Int' } -> `Node' #}
{#fun true as ^ { `Tor'  } -> `Node' #}
{#fun ones as ^ { `Tor', `Int' } -> `Node' #}
{#fun one as ^ { `Tor', `Int' } -> `Node' #}
{#fun unsigned_int as ^ { `Tor', `Int', `Int' } -> `Node' #}
{#fun int as ^ { `Tor', `Int', `Int' } -> `Node' #}

{#fun var as ^ { `Tor' , `Int', `String' } -> `Node' #}

{#fun not as ^ { `Tor' , `Node'} -> `Node' #}
{#fun neg as ^ { `Tor' , `Node'} -> `Node' #}
{#fun redor as ^ { `Tor' , `Node'} -> `Node' #}
{#fun redxor as ^ { `Tor' , `Node'} -> `Node' #}
{#fun redand as ^ { `Tor' , `Node'} -> `Node' #}
{#fun slice as ^ { `Tor' , `Node', `Int', `Int'} -> `Node' #}
{#fun uext as ^ { `Tor' , `Node', `Int'} -> `Node' #}
{#fun sext as ^ { `Tor' , `Node', `Int'} -> `Node' #}
{#fun implies as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun iff as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun xor as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun xnor as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun and as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun nand as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun or as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun nor as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun eq as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ne as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun add as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun uaddo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun saddo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun mul as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun umulo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun smulo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ult as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun slt as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ulte as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun slte as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ugt as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sgt as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ugte as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sgte as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sll as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun srl as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sra as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun rol as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ror as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sub as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun usubo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun ssubo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun udiv as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sdiv as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun sdivo as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun urem as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun srem as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun smod as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun concat as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun read as ^ { `Tor' , `Node', `Node'} -> `Node' #}
{#fun write as ^ { `Tor' , `Node', `Node', `Node'} -> `Node' #}
{#fun cond as ^ { `Tor' , `Node', `Node', `Node'} -> `Node' #}

{- TODO:

{#fun param as ^ { `Tor' , } -> `()' #}
{#fun fun as ^ { `Tor' , } -> `()' #}
{#fun uf as ^ { `Tor' , } -> `()' #}
{#fun apply as ^ { `Tor' , } -> `()' #}

-}

{#fun inc as ^ { `Tor' , `Node' } -> `Node' #}
{#fun dec as ^ { `Tor' , `Node' } -> `Node' #}

{- TODO:

{#fun match_node as ^ { `Tor' , } -> `()' #}
{#fun is_const as ^ { `Tor' , } -> `()' #}
{#fun is_var as ^ { `Tor' , } -> `()' #}
{#fun get_bits as ^ { `Tor' , } -> `()' #}
{#fun is_array as ^ { `Tor' , } -> `()' #}
{#fun is_array_var as ^ { `Tor' , } -> `()' #}
{#fun is_param as ^ { `Tor' , } -> `()' #}
{#fun is_bound_param as ^ { `Tor' , } -> `()' #}
{#fun is_fun as ^ { `Tor' , } -> `()' #}
{#fun get_fun_arity as ^ { `Tor' , } -> `()' #}
{#fun get_index_width as ^ { `Tor' , } -> `()' #}
{#fun get_symbol as ^ { `Tor' , } -> `()' #}
{#fun set_symbol as ^ { `Tor' , } -> `()' #}

-}

{#fun get_width as ^ { `Tor' , `Node' } -> `Int' #}

{#fun bv_assignment as ^ { `Tor' , `Node' } -> `String' #}


{- TODO:

{#fun free_bv_assignment as ^ { `Tor' , } -> `()' #}

{#fun array_assignment as ^ { `Tor' , } -> `()' #}
{#fun free_array_assignment as ^ { `Tor' , } -> `()' #}
{#fun uf_assignment as ^ { `Tor' , } -> `()' #}
{#fun free_uf_assignment as ^ { `Tor' , } -> `()' #}
{#fun print_model as ^ { `Tor' , } -> `()' #}
{#fun bool_sort as ^ { `Tor' , } -> `()' #}
{#fun bitvec_sort as ^ { `Tor' , } -> `()' #}
{#fun fun_sort as ^ { `Tor' , } -> `()' #}
{#fun release_sort as ^ { `Tor' , } -> `()' #}
{#fun parse as ^ { `Tor' , } -> `()' #}

-}


-- see https://www.haskell.org/pipermail/haskell-cafe/2014-December/117371.html

{#pointer *FILE as File foreign finalizer fclose newtype#}
{#fun fopen as ^ {`String', `String'} -> `File' #}

{#fun dump_btor_node as ^ { `Tor' , `File', `Node' } -> `()' #}
{#fun dump_btor as ^ { `Tor' , `File' } -> `()' #}
{#fun dump_smt1_node as ^ { `Tor' , `File', `Node' } -> `()' #}
{#fun dump_smt1 as ^ { `Tor' , `File' } -> `()' #}
{#fun dump_smt2_node as ^ { `Tor' , `File', `Node' } -> `()' #}
{#fun dump_smt2 as ^ { `Tor' , `File' } -> `()' #}

