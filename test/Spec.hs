import Prelude hiding (getLine, putStrLn, print)
import qualified Prelude
import Types
import Examples.Tasks
import Examples.Hangman
import Testing

main :: IO ()
main = do
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing solution1 against task1"
  test solution1 task1
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing solution2 against task2"
  test solution2 task2
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing solution3 against task3"
  test solution3 task3
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing solution1 against (task1 ∨ task3)"
  test solution1 (Choice [task1,task3])
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing solution2 against (task1 ∨ task3)"
  test solution2 (Choice [task1,task3])
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing solution3 against (task1 ∨ task3)"
  test solution3 (Choice [task1,task3])
  Prelude.putStrLn ""
  Prelude.putStrLn "Testing hangman"
  test (hangmanProg [2,7,1,4,2,1]) (hangmanSpec [2,7,1,4,2,1])
