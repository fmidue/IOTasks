{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.SpecGen (specGen, loopBodyGen) where

import Data.Environment (Environment, Varname)
import Test.IOTasks.ValueSet (ValueSet)
import Test.IOTasks.Specification
import Test.IOTasks.Language (exit, ints,nats, getCurrent, getAll, Pattern(..))
import Test.IOTasks.Language as Pattern (var)

import qualified Data.Term.Liftable.Prelude as T
import Data.Term.ITerm (ITerm, lit)

import Test.QuickCheck (arbitrary, Gen)
import Test.QuickCheck.GenT
import Data.List (nub, intersect)

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity

type SpecTerm = ITerm Environment Varname

specGen :: Gen (Specification SpecTerm)
specGen =
  let vs = ["m","n","x","y","z"]
      vss = [ints, nats]
  in runIdentity <$> runGenT (runReaderT (evalStateT specGen' []) (vs,vss))

loopBodyGen :: Gen (Specification SpecTerm)
loopBodyGen =
  let vs = ["m","n","x","y","z"]
      vss = [ints, nats]
  in sized $ \n -> if n <= 2
    then return exit
    else do
      ~(TillE s) <- runIdentity <$> runGenT (runReaderT (evalStateT loop []) (vs,vss))
      return s

type GenM a = StateT [Varname] (ReaderT ([Varname], [ValueSet]) (GenT Identity)) a

specGen' :: GenM (Specification SpecTerm)
specGen' = sized $ \n ->
  if n > 0
    then do
      (s, n') <- oneof $
        [simple  | n > 0 ] ++
        [complex | n >= 2]
      s' <- resize (n - n') specGen'
      return $ s <> s'
    else
      return $ Spec []

simple :: GenM (Specification SpecTerm, Int)
simple = do
  a <- oneof [input, output]
  return (Spec [a], 1)

complex :: GenM (Specification SpecTerm, Int)
complex = sized $ \n -> do
  n' <- choose (2,n)
  a <- resize n' . oneof $
    [ branch | always ] ++
    [ loop   | n' > 2 ]
  return (Spec [a], n')

input :: GenM (Action (Specification SpecTerm) SpecTerm)
input = do
  (xs,vss) <- ask
  x <- elements xs
  vs <- elements vss
  modify (nub.(x:))
  return $ ReadInput x vs

output :: GenM (Action (Specification SpecTerm) SpecTerm)
output = do
  opt <- liftGen $ arbitrary @Bool
  n <- oneof [return 1, choose (2::Int,4)]
  let ps = [ Pattern.var i <> linebreak | i <- [0 .. n-1] ]
  ts <- vectorOf n term
  return $ WriteOutput opt ps ts

term :: GenM (SpecTerm Int)
term = do
  used <- get
  oneof $ concat
    [ [ unary  | always          ]
    , [ var    | not $ null used ]
    , [ binary | not $ null used ]
    ] where
  var = do
    used <- get
    x <- elements used
    return $ getCurrent x
  unary = do
    (vs,_) <- ask
    f <- elements [T.sum, T.length]
    x <- elements vs
    return $ f $ getAll @Int x
  binary = do
    used <- get
    f <- elements [(T.+), (T.-), (T.*)]
    x <- elements used
    y <- elements used
    return $ f (getCurrent x) (getCurrent y)

branch :: GenM (Action (Specification SpecTerm) SpecTerm)
branch = sized $ \n -> do
  c <- condition
  ~[n1,n2] <- splitSizeIn 2 (n-1)
  (s1,s2) <- forkStateComp
    intersect
    (resize n1 specGen')
    (resize n2 specGen')
  return $ Branch c s1 s2

condition :: GenM (SpecTerm Bool)
condition = oneof [unary, binary] where
  unary = do
    vs <- asks fst
    x <- elements vs
    return $ T.null $ getAll @Int x
  binary = do
    op <- elements [(T.==), (T.>), (T.<)]
    t1 <- term
    t2 <- term
    return $ t1 `op` t2

-- this only generates loops with exactly one exit marker.
loop :: GenM (Action (Specification SpecTerm) SpecTerm)
loop = sized $ \n -> do
  vss <- asks snd
  ~[preLen, loopLen] <- splitSizeIn 2 (n-3)
  prefix <- resize preLen specGen'
  (c,x) <- loopCondition
  ty <- elements vss
  ~[n1, n2] <- splitSizeIn 2 loopLen
  let progress = ReadInput x ty
  (s1,s2) <- forkStateComp
    intersect
    (resize n1 specGen')
    (resize n2 specGen')
  s1' <- insert progress s1
  s <- oneof
    [ return $ Branch c s1' (s2 <> exit)
    , return $ Branch (T.not c) (s2 <> exit) s1'
    ]
  return $ TillE $ prefix <> Spec [s]

loopCondition :: GenM (SpecTerm Bool, Varname)
loopCondition = do
  vs <- asks fst
  x <- elements vs
  n <- choose (0,10)
  return (T.length (getAll @Int x) T.> lit n, x)

insert :: Action (Specification SpecTerm) SpecTerm -> Specification SpecTerm -> GenM (Specification SpecTerm)
insert a (Spec as) = do
  ix <- choose (0,length as)
  return . Spec $ take ix as ++ [a] ++ drop ix as

always :: Bool
always = True

-- splits (non-negative) n into k (non-negative) values s.t. sum [n_1,..,n_k] == n
splitSizeIn :: Int -> Int -> GenM [Int]
splitSizeIn k n = elements $ go k n where
  go :: Int -> Int -> [[Int]]
  go 1 n = [[n]]
  go j m = concat [ (i:) <$> go (j-1) (m-i) | i <- [0..m] ]

instance MonadGen m => MonadGen (ReaderT ([Varname],[ValueSet]) m) where
  liftGen g = lift $ liftGen g
  variant n = mapReaderT (variant n)
  sized f = let g r = sized (\n -> runReaderT (f n) r) in ReaderT g
  resize n = mapReaderT (resize n)
  choose p = lift $ choose p

instance MonadGen m => MonadGen (StateT [Varname] m) where
  liftGen g = lift $ liftGen g
  variant n = mapStateT (variant n)
  sized f = let g s = sized (\n -> runStateT (f n) s) in StateT g
  resize n = mapStateT (resize n)
  choose p = lift $ choose p

forkStateComp :: MonadState s m => (s -> s -> s) -> m a -> m b -> m (a,b)
forkStateComp f ma mb = do
  st <- get -- store current state
  a <- ma -- execute ma in st
  stA <- get
  put st -- restore original state
  b <- mb -- execute mb in st
  stB <- get
  put (f stA stB) -- combine resulting states of ma and mb
  return (a,b)
