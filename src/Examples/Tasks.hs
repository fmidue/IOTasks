{-# LANGUAGE TypeApplications #-}
module Examples.Tasks where

import Prelude hiding (putStrLn, getLine, print)

import IOtt
import Type

import Control.Monad (replicateM,replicateM_)

type Specification = Spec VarName

readFixedLengthList :: VarName -> NumType -> VarName -> Specification -> Specification
readFixedLengthList n ty xs = tillT $ CondChoice (Eq (Len $ V xs) (V n)) (readInput "<doNotGuessThis>" ty xs Nop) T Nop

-- read natural number n, then read n integers and sum them
task1 :: Spec VarName
task1 =
  readInput "n" NatTy "" $
  readFixedLengthList "n" IntTy "xs" $
  writeOutput [Sum (V "xs")]
  Nop

solution1 :: IOtt ()
solution1 = do
  --putStrLn "> "
  n <- read @Int <$> getLine
  --putStrLn $ "You entered " ++ show n
  xs <- replicateM n $ read @Int <$> getLine
  --putStrLn $ "Result: " ++ show (sum xs)
  putStrLn $ show (sum xs)
  --print $ sum xs

wrongSolution1 :: IOtt ()
wrongSolution1 = do
  --putStrLn "> "
  n <- read @Int <$> getLine
  --putStrLn $ "You entered " ++ show n
  replicateM_ n $ read @Int <$> getLine
  putStrLn "17"
  --putStrLn "Result: 17"

-- dList :: VarName -> [NumType] -> Specification -> Specification
-- dList xs tys = TillT (match tys) where
--   match [] = T
--   match (t:ts) = Choice (readInput "<x>" t xs $ match ts) (readInput "<x>" (Not t) xs Nop) Nop

-- -- read till last two numbers sum to 0 than count positive numbers divisible by 3
-- task2 :: Specification
-- task2 = _
--
-- solution2 :: IOtt ()
-- solution2 = go [] Nothing Nothing where
--   go ns mX mY =
--     if ((+) <$> mX <*> mY) == Just 0
--       then
--         print $ length [ x | x <- ns, x > 0, x `mod` 3 == 0 ]
--       else do
--         n <- read @Int <$> getLine
--         go (n:ns) (Just n) mX

-- read till zero then sum
task3 :: Specification
task3 =
  tillT (
    readInput "x" IntTy "xs" $
    CondChoice (V "x" `Eq` Lit 0) Nop (writeOutput [Sum $ V "xs"] T) Nop
  ) Nop

-- task3' :: Specification
-- task3' =
--   dList "xs" [Zero] $
--   writeOutput [Sum $ V "xs"] Nop

solution3 :: IOtt ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n == 0
      then print $ sum xs
      else go $ n:xs
--
-- -- read and reverse
-- task4 :: Spec
-- task4 = StepSpecs
--   [ (In ("line", line), doNothing)
--   , (Out (reverseLine, ["line"]) , displayValue)
--   ]
--
-- solution4 :: IOtt ()
-- solution4 = (reverse <$> getLine) >>= putStrLn
--
-- wrongSolution4 :: IOtt ()
-- wrongSolution4 = getLine >>= putStrLn
