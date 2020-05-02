{- Map operation to list with random number 1-9 (2-9 for mult) 1-3 times depending on difficulty selected. -}
import Cypher
import GameIO

main = do
  putStr title
  seed <- getSeed
  putStr $ "Encoding: " ++ (show seed) ++ "\n"
