module GameIO (run, getUserOp) where

import Text.Read (readMaybe)
import Data.Text (pack, strip, Text, toLower, unpack)
import System.Exit (exitSuccess)
import Data.List.Split
import System.Console.ANSI
import Cypher


title =
  "  ___  _  _  ____  _   _  ____  ____\n" ++
  " / __)( \\/ )(  _ \\( )_( )( ___)(  _ \\\n" ++
  "( (__  \\  /  )___/ ) _ (  )__)  )   /\n" ++
  " \\___) (__) (__)  (_) (_)(____)(_)\\_)\n\n\n"                         

invalidWarn = "Invalid input. Try again...\n\n"

seedGenMenu = "Enter 'q' at any time to quit.\n\nSelect seed type:\n1) [1, 2, 3]\n2) Random\n\n> "

difficultyMenu = "Enter difficulty:\n1) Easy\n2) Medium\n3) Hard\n\n> "

opMenu = "\nDecode the encoding with your cypher!\nEnter operation followed by integer...\n" ++
         "Operations: [+, -, *, /]\n" ++
         "Ex: '+ 7'. Beware integer division!\n\nEnter 'u' to undo.\n\n> "

run = do
  clearScreen
  putStr title
  s <- getSeed
  putStr "\n"
  d <- getDifficulty
  putStr "\n"

  struct <- getCypher d s
  runLoop s struct (seed $ evalCypher struct)

runLoop :: Cypher -> Cypher -> Cypher -> IO ()
runLoop seed encoding pCypher = do
                                  if (evalCypher pCypher) == (evalCypher seed)
                                  then replayPrompt seed encoding pCypher
                                  else do
                                         clearScreen
                                         putStr title
                                         putStr $ "Encoded: " ++ (show (evalCypher encoding)) ++ "\n"
                                         putStr $ "Seed:    " ++ (show (evalCypher seed)) ++ "\n\n"
                                         putStr $ "Your Result: " ++ (show (evalCypher pCypher)) ++ "\n"
                                         putStr $ "Your Cypher: " ++ (show pCypher) ++ "\n"
                                         newCypher <- getUserOp pCypher
                                         runLoop seed encoding newCypher

getUserOp :: Cypher -> IO Cypher
getUserOp pCypher = do
                      inp <- promptGuard opMenu opGuard invalidWarn
                      let tokens = splitOn " " (unpack $ strip $ pack inp)
                      if (tokens !! 0) == "u" || (tokens !! 0) == "U"
                      then unwrapCypher (return pCypher)
                      else return $ (parseOp tokens) pCypher



replayPrompt :: Cypher -> Cypher -> Cypher -> IO ()
replayPrompt seed encoding pCypher = do
                                      clearScreen
                                      putStr title
                                      putStr $ "\nCongratulations!\nYou won!\n\n"
                                      putStr $ "Encoding: "  ++ (show encoding) ++ "\n"
                                      putStr $ "Your Cypher: " ++ (show pCypher) ++ "\n\n"
                                      inp <- promptGuard "Play again? [y/n]\n\n> " 
                                                         (\ inp -> if ((strip $ toLower $ pack inp) == (pack "y") 
                                                                    || (strip $ toLower $ pack inp) == (pack "n"))
                                                                   then True 
                                                                   else False)
                                                         invalidWarn
                                      if (strip $ toLower $ pack inp) == (pack "y")
                                      then run
                                      else if (strip $ toLower $ pack inp) == (pack "n")
                                           then exitSuccess
                                           else error "How'd you get here?"                                                                            
  

parseIntM :: IO String -> IO Integer
parseIntM inp = inp >>= \ inp -> return (read inp :: Integer)

intGuard :: String -> Bool
intGuard x  = let res = readMaybe x :: Maybe Integer
              in case res of
                   Just x -> True
                   Nothing -> False

opGuard :: String -> Bool
opGuard str = let tokens = splitOn " " (unpack $ strip $ pack str) in
                if (tokens !! 0 == "u" || tokens !! 0 == "U")
                then True
                else if (length tokens == 2)
                     then let op = head tokens in
                            if (op == "+") || (op == "-") || (op == "*") || (op == "/")
                            then if intGuard (tokens !! 1)
                                 then True
                                 else False
                            else False
                     else False

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