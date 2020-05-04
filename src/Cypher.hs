module Cypher (
    applyCypher
  , randomSeed
  , randomCypher
  , Cypher(Seed)
) where

import System.Random (newStdGen, randomRs)

data Cypher = Div Integer Cypher -- `div`
            | Mul Integer Cypher -- *
            | Sub Integer Cypher -- -
            | Add Integer Cypher -- +
            | Seed [Integer]
              deriving Show

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
getCypherOp = newStdGen >>= \ g -> case (head $ randomRs (1 :: Integer, 3) g) 
                                   of
                                     1 -> return Mul
                                     2 -> return Sub
                                     3 -> return Add



