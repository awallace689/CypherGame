module GameIO (run) where

import Text.Read (readMaybe)
import Data.Text (pack, strip, Text)
import System.Exit (exitSuccess)
import Cypher


title =
  "  ___  _  _  ____  _   _  ____  ____\n" ++
  " / __)( \\/ )(  _ \\( )_( )( ___)(  _ \\\n" ++
  "( (__  \\  /  )___/ ) _ (  )__)  )   /\n" ++
  " \\___) (__) (__)  (_) (_)(____)(_)\\_)\n\n\n"                         

invalidWarn = "Invalid input. Try again...\n\n"

seedGenMenu = "Enter 'q' at any time to quit.\n\nSelect seed type:\n1) [1, 2, 3]\n2) Random\n\n> "

difficultyMenu = "Enter difficulty:\n1) Easy\n2) Medium\n3) Hard\n\n> "

run = do
  putStr title
  s <- getSeed
  putStr "\n"
  d <- getDifficulty
  putStr "\n"
  enc <- buildEncoding d (return s)

  putStr $ "Encoding: " ++ (show enc) ++ "\n"

parseIntM :: IO String -> IO Integer
parseIntM inp = inp >>= \ inp -> return (read inp :: Integer)

intGuard :: String -> Bool
intGuard x  = let res = readMaybe x :: Maybe Integer
              in case res of
                   Just x -> True
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
  inp <- parseIntM $ promptGuard seedGenMenu intGuard invalidWarn
  case inp of
    1 -> return $ seed [1, 2, 3]
    2 -> randomSeed
    _ -> do
      putStr invalidWarn
      getSeed

getDifficulty :: IO Difficulty
getDifficulty = do
                  inp <- parseIntM $ promptGuard difficultyMenu intGuard invalidWarn
                  case inp of
                    1 -> return Easy
                    2 -> return Med
                    3 -> return Hard