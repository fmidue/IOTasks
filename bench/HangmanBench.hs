import Test.IOTasks
    ( stdArgs,
      taskCheckWith,
      Args(..),
    )
import Test.IOTasks.Z3 (satPaths)
import Test.IOTasks.Constraints (constraintTree)

import HangmanSpec ( hangmanSpec, hangmanProg )

import System.Clock (Clock(Monotonic), getTime, TimeSpec (TimeSpec))
import Control.Monad (forM_)
import System.IO (stdout, BufferMode (NoBuffering), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "running full test procedure"
  simpleTime "full test short" $ testHangman short 10
  simpleTime "full test medium" $ testHangman medium 10

  putStrLn "searching for sat. paths"
  forM_ [0..2] $ \i -> do
    putStrLn $ unwords ["##",show (unfolds + i), "unfolds"]
    print =<< simpleTime "short word" (hangmanSatPaths short $ unfolds+i)
    print =<< simpleTime "medium word" (hangmanSatPaths medium $ unfolds+i)
    print =<< simpleTime "long word" (hangmanSatPaths long $ unfolds+i)
    putStrLn ""
  where
    unfolds = 8
    short = [4,8,7]
    medium = [4,8,7,9,2]
    long = [4,8,7,9,2,1,3]

testHangman :: [Integer] -> Int -> IO ()
testHangman w i = taskCheckWith Test.IOTasks.stdArgs{maxIterationUnfold = i, terminalOutput = False} (hangmanProg w) (hangmanSpec w)

hangmanSatPaths :: [Integer] -> Int -> IO Int
hangmanSatPaths w i = length <$> satPaths i to (constraintTree $ hangmanSpec w) maxSeqLength checkOverflows
  where
    to = solverTimeout stdArgs
    checkOverflows = avoidOverflows stdArgs
    maxSeqLength = solverMaxSeqLength stdArgs

simpleTime :: String -> IO a -> IO a
simpleTime label act = do
  start <- getTime Monotonic
  x <- act
  end <- getTime Monotonic
  let diff = end - start
  putStrLn $ unwords [label<>":","time:",show $ clockToSec diff, "sec"]
  pure x

clockToSec :: TimeSpec -> Double
clockToSec (TimeSpec s ns) = fromIntegral s + fromIntegral ns / (10^9)
