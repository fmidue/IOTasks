{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module SpecGen (specGen, loopBodyGen) where

import Test.IOTest.Language (exit, ints, nats)
import Test.IOTest.Specification
import Test.IOTest.ValueSet
import Test.IOTest.Environment
import Test.IOTest.Pattern
import Test.IOTest.Term hiding (sum)

import Test.QuickCheck (arbitrary, Gen)
import Test.QuickCheck.GenT
import           Data.List                      ( nub
                                                , intersect
                                                )

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity

specGen :: (Term t, Applicative t) => Gen (Specification t)
specGen =
  let vs = ["m","n","x","y","z"]
      vss = [ints, nats]
  in runIdentity <$>runGenT (runReaderT (evalStateT specGen' []) (vs,vss))

loopBodyGen :: (Term t, Applicative t) => Gen (Specification t)
loopBodyGen =
  let vs = ["m","n","x","y","z"]
      vss = [ints, nats]
  in sized $ \n -> if n <= 2
    then return exit
    else do
      ~(TillE s) <- runIdentity <$> runGenT (runReaderT (evalStateT loop []) (vs,vss))
      return s

type GenM a = StateT [Varname] (ReaderT ([Varname], [ValueSet]) (GenT Identity)) a

specGen' :: (Term t, Applicative t) => GenM (Specification t)
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

simple :: (Term t, Applicative t) => GenM ((Specification t), Int)
simple = do
  a <- oneof [input, output]
  return (Spec [a], 1)

complex :: (Term t, Applicative t) => GenM ((Specification t), Int)
complex = sized $ \n -> do
  n' <- choose (2,n)
  a <- resize n' . oneof $
    [ branch | always ] ++
    [ loop   | n' > 2 ]
  return (Spec [a], n')

input :: GenM (Action t)
input = do
  (xs,vss) <- ask
  x <- elements xs
  vs <- elements vss
  modify (nub.(x:))
  return $ ReadInput x vs

output :: (Term t, Applicative t) => GenM (Action t)
output = do
  opt <- liftGen $ arbitrary @Bool
  n <- oneof [return 1, choose (2::Int,4)]
  let ps = [ buildTermPattern $ '#':show i ++ "\n" | i <- [0 .. n-1] ]
  ts <- vectorOf n term
  return $ WriteOutput opt ps ts

term :: (Term t, Applicative t) => GenM (t Int)
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
    f <- elements [sum, length]
    x <- elements vs
    return $ f <$> getAll x
  binary = do
    used <- get
    f <- elements [(+), (-), (*)]
    x <- elements used
    y <- elements used
    return $ f <$> getCurrent x <*> getCurrent y

branch :: (Term t, Applicative t) => GenM (Action t)
branch = sized $ \n -> do
  c <- condition
  ~[n1,n2] <- splitSizeIn 2 (n-1)
  (s1,s2) <- forkStateComp
    intersect
    (resize n1 specGen')
    (resize n2 specGen')
  return $ Branch c s1 s2

condition :: (Term t, Applicative t) => GenM (t Bool)
condition = oneof [unary, binary] where
  unary = do
    vs <- asks fst
    x <- elements vs
    return $ null <$> getAll @_ @Int x
  binary = do
    op <- elements [(==), (>), (<)]
    t1 <- term
    t2 <- term
    return $ op <$> t1 <*> t2

-- this only generates loops with exactly one exit marker.
loop :: (Term t, Applicative t) => GenM (Action t)
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
    , return $ Branch (not <$> c) (s2 <> exit) s1'
    ]
  return $ TillE $ prefix <> Spec [s]

loopCondition :: (Term t, Applicative t) => GenM (t Bool, Varname)
loopCondition = do
  vs <- asks fst
  x <- elements vs
  n <- choose (0,10)
  return ((\xs i -> length xs > i) <$> getAll @_ @Int x <*> pure n, x)

insert :: Action t -> (Specification t) -> GenM (Specification t)
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
