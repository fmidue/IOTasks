{-# LANGUAGE TypeApplications #-}
module Example where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn
  ,until)

import IOTasks

--example specifications

example1 :: Specification
example1 =
  readInput "x" ints AssumeValid <>
  readInput "y" nats AssumeValid <>
  branch (currentValue "x" .>. currentValue "y")
    (writeOutput [Wildcard <> Value (currentValue "x" .+. currentValue "y") <> Wildcard , Value (currentValue "x" .-. currentValue "y") <> Wildcard] )
    (writeOutput [Wildcard <> Text "Result: " <> Value (currentValue "x" .*. currentValue "y")] )

example2 :: Specification
example2 =
  readInput "n" nats UntilValid <>
  until (length' (allValues "x") .==. currentValue "n")
    (writeOptionalOutput [Value $ currentValue "n" .-. length' (allValues "x")] <> readInput "x" ints AssumeValid) <>
  writeOutput [Value $ sum' $ allValues "x"]

example3 :: Specification
example3 =
  readInput "n" nats AssumeValid <>
  until (sum' (allValues "x") .>. currentValue "n")
    (readInput "x" ints AssumeValid) <>
  writeOutput [Value $ length' $ allValues "x"]

-- atempt at 'breaking' the solver
example4 :: Specification
example4 =
  readInput "x" nats AssumeValid <>
  until (currentValue "x" .==. product' (allValues "y"))
    (readInput "y" nats AssumeValid)

-- variable merging and multiple exits
example5 :: Specification
example5 =
  optionalTextOutput <>
  readInput "x" ints AssumeValid <>
  optionalTextOutput <>
  readInput "y" ints AssumeValid <>
  tillExit (
    branch (currentValue "x" .+. currentValue "y" .==. intLit 0 )
      exit
      (optionalTextOutput <>
       readInput "x" ints AssumeValid <>
        branch (currentValue "x" .+. currentValue "y" .==. intLit 0 )
          exit
          (optionalTextOutput <>
           readInput "y" ints AssumeValid)
    )
  ) <>
  writeOutput [Wildcard <> Value (length' $ filter' predicate $ allValues ["x","y"]) <> Wildcard]
    where predicate x = x > 0 && x `mod` 3 == 0

-- input modes
example6 :: Specification
example6 =
  readInput "x" nats Abort <>
  readInput "y" nats AssumeValid <>
  readInput "z" nats UntilValid <>
  writeOutput [Value $ sum' $ allValues ["x","y","z"] ]

-- variable merging
example7 :: Specification
example7 =
  readInput "x" ints AssumeValid <>
  readInput "y" ints AssumeValid <>
  branch (currentValue "y" .>. intLit 0)
    (readInput "x" ints AssumeValid <>
    readInput "x" ints AssumeValid)
    (readInput "y" ints AssumeValid)
    <>
  branch (currentValue ["x","y"] .>. intLit 5)
    (readInput "z" ints AssumeValid)
    (readInput "y" ints AssumeValid)
    <>
  writeOutput [Value $ currentValue ["x","z"]] <>
  writeOutput [Value $ length' $ allValues ["x","y","z"]] <>
  writeOutput [Value $ length' $ allValues ["x","y","a","z"]]

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
          print m
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
  loop 1 0

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

prog5 :: MonadTeletype m => m ()
prog5 = do
  putStr "Bitte geben Sie eine Zahl ein: "
  n <- readLn
  loop [n]
  where
    loop :: MonadTeletype m => [Integer] -> m ()
    loop (m:ms) = do putStr "Naechste Zahl bitte: "
                     n <- readLn
                     if n+m == 0
                       then do putStrLn "Die Summe der letzten beiden Eingaben war 0."
                               putStr "Anzahl der durch drei teilbaren positiven Eingaben: "
                               print $ length $ filter (\n' -> (n' > 0) && (n' `mod` 3 == 0)) (n:m:ms)
                               putStrLn "Programm beendet."
                       else do putStrLn "Die Summe war noch nicht 0."
                               loop (n:m:ms)
    loop [] = error "does not happen"

prog6 :: MonadTeletype m => m ()
prog6 = do
  x <- readLn @_ @Integer
  if x < 0
    then pure ()
    else do
      y <- readLn
      let loop = do
            v <- readLn
            if v < 0 then loop else pure v
      z <- loop
      print $ x + y + z

prog7 :: MonadTeletype m => m ()
prog7 = do
  x1 <- readLn @_ @Integer
  y1 <- readLn @_ @Integer
  st <- cond1 x1 y1
  (xs,ys,zs,c) <- cond2 st
  print c
  print $ length $ xs ++ ys ++ zs
  print $ length $ xs ++ ys ++ zs
  where
    cond1 x1 y1 =
      if y1 > 0
        then do
          x2 <- readLn @_ @Integer
          x3 <- readLn @_ @Integer
          pure ([x3,x2,x1],[y1],x3)
        else do
          y2 <- readLn @_ @Integer
          pure ([x1],[y2,y1],y2)
    cond2 (xs,ys,c) =
      if c > 5
        then do
          z <- readLn @_ @Integer
          pure (xs,ys,[z],z)
        else do
          y3 <- readLn @_ @Integer
          pure (xs,y3:ys,[],head xs)

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
  writeOutput [Value $ currentValue "x" .+. currentValue "y"]

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


-- hangman
hangmanSpec :: [Integer] -> Specification
hangmanSpec word = tillExit (
     branch (winCond $ allValues "g") (writeOutput [Text "correct!"] <> exit) mempty
  <> writeOutput [Text "Game state:"  <> Wildcard]
  <> readInput "g" digits AssumeValid
  <> branch ((currentValue "g" `isIn` listLit word) .&&. (currentValue "g" `isNotIn` allValues' "g" 1))
    (writeOptionalOutput [Text "good guess!"])
    (writeOptionalOutput [Text "wrong guess!"])
  )
  where
    winCond :: Term [Integer] -> Term Bool
    winCond g = foldr (\a b -> intLit a `isIn` g .&&. b) true word

digits :: ValueSet
digits = (Eq 0 `Union` GreaterThan 0) `Intersection` LessThen 10

hangmanProg :: MonadTeletype m => [Integer] -> m ()
hangmanProg word = go [] where
  go guessed
    | Prelude.all (`Prelude.elem` guessed) word = putStrLn "correct!"
    | otherwise = do
        putStrLn $ "Game state:" ++ printWord word guessed
        -- putStrLn "guess a number!"
        x <- read <$> getLine
        if x `Prelude.elem` word Prelude.&& x `Prelude.notElem` guessed
          then do
            putStrLn "good guess!" -- this is optional
            go (x:guessed)
          else do
            putStrLn "wrong guess!" -- this is optional
            go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed = Prelude.foldr (\x -> (++) (if x `Prelude.elem` guessed then show x ++ " " else "_ ")) "" xs
