{-# LANGUAGE LambdaCase #-}

module Cypher (
    seed
  , randomSeed
  , buildCypher
  , unwrapCypher
  , evalCypher
  , Cypher(Div, Add, Sub, Mul, Seed)
  , Difficulty(Easy, Med, Hard)
  , countMul
  , adjacentMul
) where

import System.Random (newStdGen, randomRs)

data Difficulty = Easy | Med | Hard

data Cypher = Div Integer Cypher
            | Mul Integer Cypher
            | Sub Integer Cypher
            | Add Integer Cypher
            | Seed [Integer]
              deriving (Show, Eq)

seed = Seed

applyCypher :: IO (Cypher -> Cypher) -> IO Cypher -> IO Cypher
applyCypher c1 c2 = c1 >>= \ outC -> c2 >>= \ inC -> return (outC inC)

randomSeed :: IO Cypher
randomSeed = newStdGen >>= \ g -> randomSeedGo [] (randomRs (-9, 9) g)

randomSeedGo :: [Integer] -> [Integer] -> IO Cypher
randomSeedGo xs rs = if length xs == 3 
                     then return $ Seed xs 
                     else if (not $ elem r xs) && (not $ r == 0)
                          then randomSeedGo (r:xs) (tail rs)
                          else randomSeedGo (xs) (tail rs)
                            where r = head rs

randomCypher :: IO (Cypher -> Cypher)
randomCypher = newStdGen >>= \ g -> partialApplyCypher getCypherOp (head $ randomRs (1, 12) g)

partialApplyCypher :: IO (Integer -> Cypher -> Cypher) -> Integer -> IO (Cypher -> Cypher)
partialApplyCypher c x = c >>= \ c -> return $ c x  

getCypherOp :: IO (Integer -> Cypher -> Cypher)
getCypherOp = newStdGen >>= \ g -> case (head $ randomRs (1 :: Integer, 3) g) of
                                     1 -> return Mul
                                     2 -> return Sub
                                     3 -> return Add

evalCypher :: IO Cypher -> IO Cypher
evalCypher c = c >>= \ cy -> return $ Seed $ evalCypherGo cy

evalCypherGo :: Cypher -> [Integer]
evalCypherGo c = case c of
                  (Seed xs) -> xs
                  (Div i c) -> map ((flip div) i) (evalCypherGo c)
                  (Mul i c) -> map (* i)          (evalCypherGo c)
                  (Sub i c) -> map ((flip (-)) i) (evalCypherGo c)
                  (Add i c) -> map (+ i)          (evalCypherGo c)

unwrapCypher :: IO Cypher -> IO Cypher
unwrapCypher c = c >>= \ case
                           (Seed xs) -> return $ Seed xs
                           (Div i c) -> return $ c
                           (Mul i c) -> return $ c 
                           (Sub i c) -> return $ c
                           (Add i c) -> return $ c

buildCypher :: Difficulty -> IO Cypher -> IO Cypher
buildCypher Easy c = do 
                      muls <- countMul cypher
                      if muls > 0
                      then cypher
                      else buildCypher Easy c
                        where
                          cypher = newStdGen >>= \ g -> buildCypherGo (head $ randomRs (2, 3) g) c
buildCypher Med  c = do
                      muls <- countMul cypher
                      adjMuls <- adjacentMul cypher
                      if muls > 1 && adjMuls == 0
                      then cypher
                      else buildCypher Med c
                        where
                          cypher = newStdGen >>= \ g -> buildCypherGo (head $ randomRs (3, 4) g) c
buildCypher Hard c = do
                      muls <- countMul cypher
                      adjMuls <- adjacentMul cypher
                      if muls > 2 && adjMuls == 0
                      then cypher
                      else buildCypher Hard c
                        where
                          cypher = newStdGen >>= \ g -> buildCypherGo (head $ randomRs (4, 5) g) c

buildCypherGo :: Integer -> IO Cypher -> IO Cypher
buildCypherGo 0 m = m
buildCypherGo i m = do 
                      outer <- randomCypher
                      c     <- m
                      buildCypherGo (i - 1) (return $ outer c)

countMul :: IO Cypher -> IO Integer
countMul c = c >>= \ c -> return (countMulGo c 0)

countMulGo :: Cypher -> Integer -> Integer
countMulGo (Mul _ c) i = countMulGo c (i + 1)
countMulGo (Div _ c) i = countMulGo c i
countMulGo (Add _ c) i = countMulGo c i
countMulGo (Sub _ c) i = countMulGo c i
countMulGo (Seed xs) i = i

adjacentMul :: IO Cypher -> IO Integer
adjacentMul c = c >>= \ c -> return (adjacentMulGo c 0)

adjacentMulGo :: Cypher -> Integer -> Integer
adjacentMulGo (Mul _ (Mul _ c)) i = adjacentMulGo c (i + 1)
adjacentMulGo (Mul _ c) i         = adjacentMulGo c i
adjacentMulGo (Div _ c) i         = adjacentMulGo c i
adjacentMulGo (Add _ c) i         = adjacentMulGo c i
adjacentMulGo (Sub _ c) i         = adjacentMulGo c i
adjacentMulGo (Seed xs) i         = i