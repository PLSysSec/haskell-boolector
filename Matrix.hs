{-# language TupleSections #-}

-- | just for demonstration

import qualified Boolector as B

import Control.Applicative
import Control.Monad

import TPDB.Input
import TPDB.Pretty
import qualified TPDB.Data as T

import qualified Data.Map as M
import Data.List ( transpose, nub, intersperse )

import System.IO

import System.Environment

main = do
    [ file ] <- getArgs
    sys <- get_srs file
    print $ pretty sys
    handle 3 $ T.rules sys

handle bits [] = putStrLn "done"
handle bits us = do    
    (int,out) <- removes bits us
    print int
    print $ pretty out
    handle bits $ do (False, u) <- out ; return u
    
removes bits us = do
    let work dim = do
          int <- remove dim bits us
          case int of
              Nothing -> work $ succ dim
              Just out -> return out
    work 1

remove dim bits us = do
  hPutStrLn stderr $ unwords
    [ "remove", "dim", show dim, "bits", show bits ]
  B.withBoolector $ do
    int <- inter (sigma us) dim bits
    (ok, sws) <- compatible dim bits int us
    mo <- monotone int
    B.assert =<< ( mo B.&& ok )
    let basename = concat $ intersperse "-"
          [ "t", show $ length us , show dim, show bits ] 
    B.dumpBtor $ basename ++ ".btor"
    B.dumpSmt1 $ basename ++ ".smt1"
    B.dumpSmt2 $ basename ++ ".smt2"
    B.withSolution $ do
        i <- rinter int
        us <- forM sws $ \ (s, w, u) -> do
          ss <- B.bval s
          return (ss, u)
        return (i, us)

sigma us = nub $ do u <- us ; T.lhs u ++ T.rhs u
  
monotone int = mo B.and $ for (M.elems int) $ \ m ->
  do tl <- positive (topleft m) ; br <- positive (botright m)
     tl B.&& br

positive v = B.redor v

compatible dim bits int us = do
  sws <- forM us $ \ u -> do
      l <- eval dim bits int $ T.lhs u
      r <- eval dim bits int $ T.rhs u
      s <- strictly_greater l r
      w <- weakly_greater l r
      return (s, w, u)
  allweak <- B.and $ for sws $ \ (s,w,u) -> w
  somestrict <- B.or $ for sws $ \ (s,w,u) -> s
  good <- allweak B.&& somestrict
  return (good , sws )

eval dim bits int s = do
  unit <- forM [1..dim] $ \ i -> forM [1..dim] $ \ j ->
       B.unsignedInt ( if i == j then 1 else 0 ) bits
  foldM mtimes unit $ for s ( int M.! )
      

topleft m = head $ head m
topright m = last $ head m
botright m = last $ last m


strictly_greater l r = B.ugt (topright l) (topright r)

weakly_greater l r = 
  mo B.and $ concat
      $ for ( zip l r ) $ \ (xs, ys) ->
        for ( zip xs ys) $  \ (x,y) -> B.ugte x y

mo f xs = sequence xs >>= f

for = flip map

rmatrix m = forM m $ \ row -> forM row $ \ n -> B.val n

matrix_ dim bits = do
    forM [ 1 .. dim ] $ \ i ->
      forM [ 1 .. dim ] $ \ j ->
        natural bits

natural bits = B.var bits

matrix dim bits = do
    forM [ 1 :: Int .. dim ] $ \ i ->
      forM [ 1 :: Int .. dim ] $ \ j ->
        if j == 1
        then B.unsignedInt (if i == 1 then 1 else 0) bits
        else if i == dim
        then B.unsignedInt (if j == dim then 1 else 0) bits
        else B.var bits

mtimes a b = 
  forM a $ \ row ->
    forM (transpose b) $ \ col -> do
       p : ps <- sequence $ zipWith times row col
       foldM plus p ps

times x y = do
    res <- B.mul x y
    o <- B.umulo x y
    B.assert =<< B.not o
    return res

plus x y = do
    res <- B.add x y
    o <- B.uaddo x y
    B.assert =<< B.not o
    return res

inter sigma dim bits = M.fromList
   <$> (forM sigma $ \ c -> (c,) <$> matrix dim bits)
                                     
rinter int = M.fromList <$> forM (M.toList int) (\ (k,v) ->
     (k,) <$> rmatrix v )
