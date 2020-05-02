{- Map operation to list with random number 1-9 (2-9 for mult) 1-3 times depending on difficulty selected. -}
import System.Random (newStdGen, randomRs)
import Text.Read (readMaybe)
import Data.Text (pack, strip, Text)
import System.Exit (exitSuccess)

title =
  "  ___  _  _  ____  _   _  ____  ____\n" ++
  " / __)( \\/ )(  _ \\( )_( )( ___)(  _ \\\n" ++
  "( (__  \\  /  )___/ ) _ (  )__)  )   /\n" ++
  " \\___) (__) (__)  (_) (_)(____)(_)\\_)\n\n\n"                         

invalidWarn = "Invalid input. Try again...\n\n"

seedGenMenu = "Enter 'q' at any time to quit.\n\nSelect seed type:\n1) [1, 2, 3]\n2) Random\n> "

data Cypher = Div Int Cypher -- `div`
            | Mul Int Cypher -- *
            | Sub Int Cypher -- -
            | Add Int Cypher -- +
            | Seed [Int]
              deriving Show

randomSeed :: IO Cypher
randomSeed = newStdGen >>= \ g -> randomSeedGo ([] :: [Int]) (randomRs (-9, 9) g)

randomSeedGo :: [Int] -> [Int] -> IO Cypher
randomSeedGo xs rs = if length xs == 3 
                     then return $ Seed xs 
                     else if not $ elem r xs
                          then randomSeedGo (r:xs) (tail rs)
                          else randomSeedGo (xs) (tail rs)
                            where r = head rs

parseInt :: IO String -> IO Int
parseInt inp = inp >>= \ inp -> return (read inp :: Int)

intGuard :: String -> Bool
intGuard x  = let res = readMaybe x :: Maybe Int
              in case res of
                   Just x -> True
                   Nothing -> False

intQuitGuard :: String -> Bool
intQuitGuard x = let res = readMaybe x :: Maybe Int
                 in case res of
                      Just x  -> True
                      Nothing -> False

prompt :: String -> IO String
prompt p = putStr p >> getLine

promptGuard :: String -> (String -> Bool) -> String -> IO String
promptGuard p f w = (putStr p) >> 
                    getLine    >>= \ inp -> if (strip $ pack inp) == (pack "q")
                                            then exitSuccess
                                            else if f inp
                                                 then return inp
                                                 else putStr w >> promptGuard p f w
                    
getSeed :: IO Cypher
getSeed = do
  inp <- parseInt $ promptGuard seedGenMenu intGuard invalidWarn
  case inp of
    1 -> return $ Seed [1, 2, 3]
    2 -> randomSeed
    _ -> do
      putStr invalidWarn
      getSeed

main = do
  putStr title
  seed <- getSeed
  putStr $ "Encoding: " ++ (show seed) ++ "\n"
