import Cypher
import GameIO

main = do
  putStr title
  seed <- getSeed
  putStr $ "Encoding: " ++ (show seed) ++ "\n"
