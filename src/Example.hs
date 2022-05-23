{-# LANGUAGE TypeApplications #-}
module Example where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn
  ,until, all)


import IOTasks

--example specifications

example1 :: Specification
example1 =
  readInput "x" ints AssumeValid <>
  readInput "y" nats AssumeValid <>
  branch (Current "x" :>: Current "y")
    (writeOutput [Wildcard <> Value (current "x" +# current "y") <> Wildcard , Value (current "x" -# current "y") <> Wildcard] )
    (writeOutput [Wildcard <> Text "Result: " <> Value (current "x" *# current "y")] )

example2 :: Specification
example2 =
  readInput "n" nats AssumeValid <>
  until (Length (All "x") :==: Current "n")
    (writeOptionalOutput [Value $ current "n" -# length' (all "x")] <> readInput "x" ints AssumeValid) <>
  writeOutput [Value $ sum' $ all "x"]

example3 :: Specification
example3 =
  readInput "n" nats AssumeValid <>
  until (Sum (All "x") :>: Current "n")
    (readInput "x" ints AssumeValid) <>
  writeOutput [Value $ length' $ all "x"]

-- atempt at 'breaking' the solver
example4 :: Specification
example4 =
  readInput "x" nats AssumeValid <>
  until (Current "x" :==: Product (All "y"))
    (readInput "y" nats AssumeValid)

ints, nats :: ValueSet
ints = Every
nats = Eq 0 `Union` GreaterThan 0

-- example programs

prog1 :: MonadTeletype m => m ()
prog1 = do
  x <- readLn @_ @Integer
  y <- readLn
  if x > y
    then print $ x + y
    else putStr "Result: " >> print (x * y)

prog2 :: MonadTeletype m => m ()
prog2 = do
  n <- readLn @_ @Integer
  if n < 0
    then prog2
    else
      let
        loop 0 x = print @_ @Integer x
        loop m x = do
          -- putChar m
          i <- readLn
          loop (m-1) (x+i)
      in loop n 0

prog2' :: MonadTeletype m => m ()
prog2' = do
  n <- readLn @_ @Integer
  let
    loop m x
      | m >= n = print @_ @Integer x
      | otherwise = do
        i <- readLn
        loop (m+1) (x+1+i)
  loop 0 0

prog3 :: MonadTeletype m => m ()
prog3 = do
  n <- readLn @_ @Integer
  let
    loop s m
      | s > n  = print @_ @Integer m
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0

prog4 :: MonadTeletype m => m ()
prog4 = do
  x <- readLn @_ @Integer
  let
    loop p
      | p == x = pure ()
      | otherwise = do
        y <- readLn
        loop (p*y)
  loop 1

--
stringS1 :: Specification
stringS1 = writeOutput [Text "A"] <> writeOutput [Text "B"]

stringS2 :: Specification
stringS2 = writeOutput [Text "A" <> Text "B"]

stringP1 :: MonadTeletype m => m ()
stringP1 = putStrLn "AB"

stringP2 :: MonadTeletype m => m ()
stringP2 = putStrLn "A" >> putStrLn "B"

stringP3 :: MonadTeletype m => m ()
stringP3 = putStr "A" >> putStr "B"

---

addSpec :: Specification
addSpec =
  readInput "x" nats AssumeValid <>
  readInput "y" nats AssumeValid <>
  writeOutput [Value $ current "x" +# current "y"]

nonsense :: MonadTeletype m => m ()
nonsense = do
  putStrLn =<< plus <$> getLine <*> getLine

plus :: String -> String -> String
plus x y = reverse $ plus' (reverse $ filter (>= '0') x) (reverse $ filter (>= '0') y) where
  plus' xs [] = xs
  plus' [] ys = ys
  plus' ('0':xs) (y:ys) = y : plus' xs ys
  plus' [x] ('9':ys) = plus' (pred x:['1']) ('0':ys)
  plus' (x:x':xs) ('9':ys) = plus' (pred x:succ x':xs) ('0':ys)
  plus' (x:xs) (y:ys) = plus' (pred x:xs) (succ y:ys)
