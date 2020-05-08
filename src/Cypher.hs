{-# LANGUAGE LambdaCase #-}

module Cypher (
    seed
  , randomSeed
  , getCypher
  , unwrapCypher
  , evalCypher
  , Cypher()
  , Difficulty(Easy, Med, Hard)
  , parseOp
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

evalCypher :: Cypher -> [Integer]
evalCypher c = case c of
                (Seed xs) -> xs
                (Div i c) -> map ((flip div) i) (evalCypher c)
                (Mul i c) -> map (* i)          (evalCypher c)
                (Sub i c) -> map ((flip (-)) i) (evalCypher c)
                (Add i c) -> map (+ i)          (evalCypher c)

unwrapCypher :: IO Cypher -> IO Cypher
unwrapCypher c = c >>= \ case
                           (Seed xs) -> return $ Seed xs
                           (Div i c) -> return $ c
                           (Mul i c) -> return $ c 
                           (Sub i c) -> return $ c
                           (Add i c) -> return $ c

getCypher:: Difficulty -> Cypher -> IO Cypher
getCypher Easy c = do 
                      cypher <- newStdGen >>= \ g -> buildCypher (head $ randomRs (2, 3) g) c
                      let muls = countMul cypher
                      let adjMuls = adjacentMul cypher
                      if (muls > 0)
                      then return cypher
                      else getCypher Easy c

getCypher Med  c = do
                      cypher <- newStdGen >>= \ g -> buildCypher (head $ randomRs (3, 4) g) c
                      let muls = countMul cypher
                      let adjMuls = adjacentMul cypher
                      if (muls > 1) && (adjMuls == 0)
                      then return cypher
                      else getCypher Med c

getCypher Hard c = do
                      cypher <- newStdGen >>= \ g -> buildCypher (head $ randomRs (4, 5) g) c
                      let muls = countMul cypher
                      let adjMuls = adjacentMul cypher
                      if (muls > 2) && (adjMuls == 0)
                      then return cypher
                      else getCypher Hard c
          
parseOp :: [String] -> (Cypher -> Cypher)
parseOp tokens = case tokens !! 0 of
                   "+" -> Add (read (tokens !! 1) :: Integer)
                   "-" -> Sub (read (tokens !! 1) :: Integer)
                   "*" -> Mul (read (tokens !! 1) :: Integer)
                   "/" -> Div (read (tokens !! 1) :: Integer)

buildCypher :: Integer -> Cypher -> IO Cypher
buildCypher 0 c = return c
buildCypher i c = do 
                      outer <- randomCypher
                      buildCypher (i - 1) (outer c)

countMul :: Cypher -> Integer
countMul c = countMulGo c 0

countMulGo :: Cypher -> Integer -> Integer
countMulGo (Mul _ c) i = countMulGo c (i + 1)
countMulGo (Div _ c) i = countMulGo c i
countMulGo (Add _ c) i = countMulGo c i
countMulGo (Sub _ c) i = countMulGo c i
countMulGo (Seed xs) i = i

adjacentMul :: Cypher -> Integer
adjacentMul c = adjacentMulGo c 0

adjacentMulGo :: Cypher -> Integer -> Integer
adjacentMulGo (Mul _ (Mul _ c)) i = adjacentMulGo c (i + 1)
adjacentMulGo (Mul _ c) i         = adjacentMulGo c i
adjacentMulGo (Div _ c) i         = adjacentMulGo c i
adjacentMulGo (Add _ c) i         = adjacentMulGo c i
adjacentMulGo (Sub _ c) i         = adjacentMulGo c i
adjacentMulGo (Seed xs) i         = i