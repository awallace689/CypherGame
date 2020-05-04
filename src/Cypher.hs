{-# LANGUAGE LambdaCase #-}
module Cypher (
    applyCypher
  , randomSeed
  , randomCypher
  , evalCypher
  , Cypher()
  , Difficulty(Easy, Med, Hard)
  , buildEncoding
  , unwrapCypher
  , seed
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

buildEncoding :: Difficulty -> IO Cypher -> IO Cypher
buildEncoding Easy c = do { g <- newStdGen ; buildEncodingGo (head $ randomRs (2, 3) g) c }
buildEncoding Med  c = do { g <- newStdGen ; buildEncodingGo (head $ randomRs (3, 4) g) c }
buildEncoding Hard c = do { g <- newStdGen ; buildEncodingGo (head $ randomRs (4, 5) g) c }

buildEncodingGo :: Integer -> IO Cypher -> IO Cypher
buildEncodingGo 0 m = m
buildEncodingGo i m = do 
                        outer <- randomCypher
                        c   <- m
                        buildEncodingGo (i - 1) (return $ outer c)
