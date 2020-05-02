module Cypher (
    applyCypher
  , randomSeed
  , Cypher(Seed)
) where

import System.Random (newStdGen, randomRs)

data Cypher = Div Int Cypher -- `div`
            | Mul Int Cypher -- *
            | Sub Int Cypher -- -
            | Add Int Cypher -- +
            | Seed [Int]
              deriving Show

applyCypher :: IO (Cypher -> Cypher) -> IO Cypher -> IO Cypher
applyCypher c1 c2 = c1 >>= \ outC -> c2 >>= \ inC -> return (outC inC)

randomSeed :: IO Cypher
randomSeed = newStdGen >>= \ g -> randomSeedGo [] (randomRs (-9, 9) g)

randomSeedGo :: [Int] -> [Int] -> IO Cypher
randomSeedGo xs rs = if length xs == 3 
                     then return $ Seed xs 
                     else if (not $ elem r xs) && (not $ r == 0)
                          then randomSeedGo (r:xs) (tail rs)
                          else randomSeedGo (xs) (tail rs)
                            where r = head rs
