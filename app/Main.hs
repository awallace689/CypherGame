{- Map operation to list with random number 1-9 (2-9 for mult) 1-3 times depending on difficulty selected. -}
import System.Random
import Text.Read

title =
  "  ___  _  _  ____  _   _  ____  ____\n" ++
  " / __)( \\/ )(  _ \\( )_( )( ___)(  _ \\\n" ++
  "( (__  \\  /  )___/ ) _ (  )__)  )   /\n" ++
  " \\___) (__) (__)  (_) (_)(____)(_)\\_)\n\n\n"                         

invalidWarn = "Invalid input. Try again...\n"

data Cypher = Div Int Cypher -- `div`
            | Mul Int Cypher -- *
            | Sub Int Cypher -- -
            | Add Int Cypher -- +
            | Seed [Int] 
              deriving Show

parseInt :: IO String -> IO Int
parseInt inp = inp >>= \ inp -> return (read inp :: Int)

intGuard :: String -> Bool
intGuard x  = let res = readMaybe x :: Maybe Int
              in case res of
                Just x -> True
                Nothing -> False

prompt :: String -> IO String
prompt p = putStr p >> getLine

promptGuard :: String -> (String -> Bool) -> IO String
promptGuard p f = (putStr p) >> getLine >>= \ inp -> if f inp
                                                     then return inp
                                                     else putStr invalidWarn >> promptGuard p f
                    
getSeed :: IO Int -> IO Cypher
getSeed input = input >>= \ x -> case x of 
                                      1 -> return $ Seed [1, 2, 3]
                                      --2 -> return genRandomSeed
                                      _ -> do
                                        putStr invalidWarn
                                        getSeed $ parseInt $ promptGuard "> " intGuard

main = do
  putStr title
  putStr "Input '1'.\n"
  seed <- getSeed $ parseInt $ promptGuard "> " intGuard 
  putStr $ "Encoding: " ++ (show seed) ++ "\n"
