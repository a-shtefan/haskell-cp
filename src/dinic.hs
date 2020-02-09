-- Dinic's algorithm to find maximum flow in a network.
-- Use `build` to build a network from a list of edges [(u, v, cap)]
-- representing u -> v with capacity cap, and then `dinic` to get maximum flow
-- in the network. `dinic` can be easily modified to return flow for each edge
-- by returning `flows`.
-- This implementation uses ST monad to implement fast DFS and BFS.
-- The implementation is close to C++ implementation which can be found here:
-- https://cp-algorithms.com/graph/dinic.html
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.ST.Safe
import Data.Array ((!))
import qualified Data.Array as A
import Data.Array.ST.Safe
import Data.Bits
import Data.List
import Data.Sequence (ViewL((:<)), (|>))
import qualified Data.Sequence as Sq
import Data.STRef

whenM cond body = cond >>= (flip when body)
while cond body = whenM cond (body >> while cond body)
updArr arr i fn = readArray arr i >>= (writeArray arr i . fn)
(<&&>) a b = (&&) <$> a <*> b

data Graph a = Graph { neighbors :: A.Array Int [(Int, Int)]
                     , caps :: A.Array Int a
                     } deriving Show

addEdge g u v w = updArr g u ((v, w):)

build :: Num a => Int -> [(Int, Int, a)] -> Graph a
build n edges =
  let caps_arr = A.listArray (0, 2 * length edges - 1) $
                     intersperse 0 (map (\(_, _, w) -> w) edges) ++ [0]
      g = runSTArray $ do
        g <- newArray (1, n) []
        forM_ (edges `zip` [0 ..]) $ \((u, v, _), i) -> do
          addEdge g u v $ 2 * i
          addEdge g v u $ 2 * i + 1
        return g
  in Graph { neighbors = g, caps = caps_arr }

dinicBfs (Graph neighbors caps) s t flows d = do
  forM_ [1 .. snd $ A.bounds neighbors] $ \i -> writeArray d i maxBound
  writeArray d s 0
  q <- newSTRef $ Sq.singleton (s, 0)
  while ((not . Sq.null) <$> readSTRef q) $ do
    (v, dv) :< q_rest <- Sq.viewl <$> readSTRef q
    writeSTRef q q_rest
    forM_ (neighbors ! v) $ \(u, i) -> do
      du <- readArray d u
      e_flow <- readArray flows i
      when (caps ! i > e_flow && du == maxBound) $ do
        writeArray d u $ dv + 1
        modifySTRef q (|> (u, dv + 1))
  dt <- readArray d t
  return $ dt < maxBound

dinicDfs g@(Graph _ caps) v flow t d flows ptrs = do
  if v == t || flow == 0 then return flow
  else do
    pushed <- newSTRef 0
    dv <- readArray d v
    let not_pushed = (== 0) <$> readSTRef pushed
    let ptr_not_empty = (not . null) <$> readArray ptrs v
    while (not_pushed <&&> ptr_not_empty) $ do
      (u, i) : _ <- readArray ptrs v
      du <- readArray d u
      remaining <- (caps ! i -) <$> readArray flows i
      when (du == dv + 1 && remaining > 0) $ do
        fp <- dinicDfs g u (min flow remaining) t d flows ptrs
        when (fp > 0) $ do
          updArr flows i (+ fp)
          updArr flows (i `xor` 1) (+ negate fp)
          writeSTRef pushed fp
      whenM (not_pushed <&&> ptr_not_empty) $ updArr ptrs v tail
    readSTRef pushed

dinic :: (Ord a, Num a, Bounded a) => Graph a -> Int -> Int -> a
dinic g@(Graph neighbors caps) s t = runST $ do
  flows <- newArray (A.bounds caps) 0 :: Num a => ST s (STArray s Int a)
  d <- newArray (A.bounds neighbors) 0 :: ST s (STArray s Int Int)
  flow <- newSTRef 0
  while (dinicBfs g s t flows d) $ do
    ptrs <- thaw neighbors :: ST s (STArray s Int [(Int, Int)])
    pushed <- newSTRef maxBound
    while ((> 0) <$> readSTRef pushed) $ do
      fp <- dinicDfs g s maxBound t d flows ptrs
      modifySTRef flow (+ fp)
      writeSTRef pushed fp
  readSTRef flow

-- End cut here.
-- SWERC 2015 problem F - Landscaping

main = interact $ show . solve . lines

solve (ns : l_field) = dinic g s t
  where [n, m, a, b] = map read $ words ns
        s = n * m + 1
        t = n * m + 2
        edges =
          [(s, (i-1)*m+j, b) | i<-[1..n], j<-[1..m], field ! i ! j == '.'] ++
          [((i-1)*m+j, t, b) | i<-[1..n], j<-[1..m], field ! i ! j == '#'] ++
          [((i-1)*m+j, (i-2)*m+j, a) | i<-[2..n], j<-[1..m]] ++
          [((i-1)*m+j, i*m+j, a) | i<-[1..n-1], j<-[1..m]] ++
          [((i-1)*m+j, (i-1)*m+j-1, a) | i<-[1..n], j<-[2..m]] ++
          [((i-1)*m+j, (i-1)*m+j+1, a) | i<-[1..n], j<-[1..m-1]]
        g = build (n * m + 2) edges
        field = A.listArray (1, n) $ map (A.listArray (1, m)) $ take n l_field
