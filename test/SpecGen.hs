{-# LANGUAGE TypeApplications #-}
module SpecGen (specGen) where

import Test.IOTest.Language (exit)
import Test.IOTest.Internal.Specification
import Test.IOTest.Internal.ValueSet
import Test.IOTest.Internal.Environment
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Term

import Test.QuickCheck hiding (output)
import           Data.List                      ( nub )

specGen :: Gen Specification
specGen = do
  let vs = ["m","n","x","y","z"]
      vss = [ValueSet [(0::Int)..10], ValueSet [(-10 :: Int)..10] ]
  specGen' vs [] vss

specGen' :: [Varname] -> [Varname] -> [ValueSet] -> Gen Specification
specGen' vs used vss = sized $ \n ->
  if n > 0
    then do
      (s, n', used') <- oneof $
        [simple vs used vss  | n > 0 ] ++
        [complex vs used vss | n >= 2]
      s' <- scale (subtract n') (specGen' vs used' vss)
      return $ s <> s'
    else
      return $ Spec []

simple :: [Varname] ->  [Varname] -> [ValueSet] -> Gen (Specification, Int, [Varname])
simple vs used vss = do
  (a, used') <- oneof [input vs used vss, output vs used]
  return (Spec [a], 1, used')

complex :: [Varname] -> [Varname] -> [ValueSet] -> Gen (Specification, Int, [Varname])
complex vs used vss = sized $ \n -> do
  n' <- choose (2,n)
  a <- resize n' $ oneof $
    [ branch vs used vss | always ] ++
    [ loop vs used vss   | n' > 2  ]
  return (Spec [a], n', used)

input :: [Varname] -> [Varname] -> [ValueSet] -> Gen (Action, [Varname])
input xs used vss = do
  x <- elements xs
  vs <- elements vss
  return (ReadInput x vs, nub $ x:used)

output :: [Varname] -> [Varname] -> Gen (Action, [Varname])
output vs used = do
  opt <- arbitrary @Bool
  n <- oneof [return 1, choose @Int (2,4)]
  let ps = [ buildPattern $ '#':show i | i <- [0 .. n-1] ]
  ts <- vectorOf n (term vs used)
  return (WriteOutput opt ps ts, used)

term :: [Varname] -> [Varname] -> Gen (Term Int)
term vs used = oneof $ concat
  [ [ unary  | always          ]
  , [ var    | not $ null used ]
  , [ binary | not $ null used ]
  ] where
  var = do
    x <- elements used
    return $ getCurrent x
  unary = do
    f <- elements [sum, length]
    x <- elements vs
    return $ f <$> getAll x
  binary = do
    f <- elements [(+), (-), (*)]
    x <- elements used
    y <- elements used
    return $ f <$> getCurrent x <*> getCurrent y

branch :: [Varname] -> [Varname] -> [ValueSet] -> Gen Action
branch vs used vss = sized $ \n -> do
  c <- condition vs used
  let (n1,n2) = splitSize (n-1)
  s1 <- resize n1 (specGen' vs used vss)
  s2 <- resize n2 (specGen' vs used vss)
  return $ Branch c s1 s2

-- maybe randomize this too
splitSize :: Int -> (Int, Int)
splitSize n | even n    = let n' = div n 2 in (n',n')
            | otherwise = let n' = div n 2 in (n', n'+1)

condition :: [Varname] -> [Varname] -> Gen (Term Bool)
condition vs used = oneof [unary, binary] where
  unary = do
    x <- elements vs
    return $ null <$> getAll @Int x
  binary = do
    op <- elements [(==), (>), (<)]
    t1 <- term vs used
    t2 <- term vs used
    return $ op <$> t1 <*> t2

-- this only generates loops with exactly one exit marker.
loop :: [Varname] -> [Varname] -> [ValueSet] -> Gen Action
loop vs used vss = sized $ \n -> do
  pLen <- choose @Int (0,n - 3)
  prefix <- resize pLen (specGen' vs used vss)
  (c,x) <- loopCondition vs
  ty <- elements vss
  let (n1, n2) = splitSize $ n - pLen - 3
      progress = ReadInput x ty
  s1 <- resize n1 (specGen' vs used vss)
  s2 <- resize n2 (specGen' vs used vss)
  s1' <- insert progress s1
  let s = Branch c s1' (s2 <> exit)
  return $ TillE $ prefix <> Spec [s]

loopCondition :: [Varname] -> Gen (Term Bool, Varname)
loopCondition vs = do
  x <- elements vs
  n <- choose @Int (0,10)
  return ((\xs i -> length xs > i) <$> getAll @Int x <*> pure n, x)

insert :: Action -> Specification -> Gen Specification
insert a (Spec as) = do
  ix <- choose (0,length as)
  return . Spec $ take ix as ++ [a] ++ drop ix as

always :: Bool
always = True
