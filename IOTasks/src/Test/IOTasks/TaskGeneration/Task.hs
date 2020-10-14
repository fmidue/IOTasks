{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.IOTasks.TaskGeneration.Task where

import Test.QuickCheck

import Control.Arrow

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

instance Contravariant Require where
  contramap f (Require r) = Require (r . f)

instance Divisible Require where
  divide f r s = Require $
    \x -> let (y,z) = f x in check r y .&&. check s z
  conquer = Require $ const (property True)

instance Contravariant TaskInstance where
  contramap f (TaskInstance d r) = TaskInstance d (contramap f r)

instance Contravariant TaskDesign where
  contramap f (TaskDesign g fb) = TaskDesign g (contramap f . fb)

type Description = PP.Doc

data TaskInstance s = TaskInstance
    { question :: Description
    , requires :: Require s
    }

newtype Require s = Require { check :: s -> Property}

exactAnswer :: (Eq a, Show a) => a -> Require a
exactAnswer x = Require $ \s -> s === x

data TaskDesign s = forall p. TaskDesign
  { parameter :: Gen p
  , instantiate :: p -> TaskInstance s
  }

runTaskIO :: TaskDesign s -> IO s -> IO ()
runTaskIO task getAnswer = do
  TaskInstance q req <- generateTaskInstance task
  putStrLn $ PP.render q
  s <- getAnswer
  quickCheck $ check req s

showTaskInstance :: TaskDesign s -> IO ()
showTaskInstance t = do
  i <- generateTaskInstance t
  putStrLn . PP.render $ question i

generateTaskInstance :: TaskDesign s -> IO (TaskInstance s)
generateTaskInstance (TaskDesign gen inst) =
  generate $ inst <$> gen

fixed :: p -> Gen p
fixed = pure

for :: Gen p -> (p -> TaskInstance s) -> TaskDesign s
for = TaskDesign

solveWith :: Description -> Require s -> TaskInstance s
solveWith = TaskInstance

(/\) :: Require a -> Require b -> Require (a,b)
(/\) = divided

infixr 3 &&&&
(&&&&) :: Monad m => (a -> m c) -> (a -> m c') -> a -> m (c, c')
f &&&& g = runKleisli $ Kleisli f &&& Kleisli g

infixr 3 &&&^
(&&&^) :: Monad m => (a -> m c) -> (a -> c') -> a -> m (c, c')
f &&&^ g = f &&&& (pure . g)

infixr 3 ^&&&
(^&&&) :: Monad m => (a -> c) -> (a -> m c') -> a -> m (c, c')
f ^&&& g =  (pure . f) &&&& g

from :: (a -> Gen b) -> Gen a -> Gen b
from = (=<<)

infixr 3 ****
(****) :: Monad m => (b -> m c) -> (b' -> m c') -> (b, b') -> m (c, c')
f **** g = runKleisli $ Kleisli f *** Kleisli g

infixr 3 ***^
(***^) :: Monad m => (b -> m c) -> (a -> c') -> (b, a) -> m (c, c')
f ***^ g = f **** (pure . g)

infixr 3 ^***
(^***) :: Monad m => (a -> c) -> (b' -> m c') -> (a, b') -> m (c, c')
f ^*** g = pure . f **** g

fromBoth :: (a -> Gen b) -> Gen (a,a) -> Gen (b,b)
fromBoth f g = (f **** f) =<< g

multipleChoice :: Show a => Int -> [a] -> [a] -> Gen (Description, [Int])
multipleChoice n rs ws = do
  let  rs' = map (,True) rs
       ws' = map (,False) ws
  cs <- take n <$> shuffle (rs' ++ ws')
  let  desc = PP.vcat $ zipWith (\i x -> PP.text $ show i ++ ") " ++ show x) [1..] (map fst cs)
       is = [ i | (i,(_,correct)) <- zip [1..] cs, correct ]
  return (desc,is)

multipleChoicePP :: Pretty a => Int -> [a] -> [a] -> Gen (Description, [Int])
multipleChoicePP n rs ws = do
  let  rs' = map (,True) rs
       ws' = map (,False) ws
  cs <- take n <$> shuffle (rs' ++ ws')
  let  desc = PP.vcat $ zipWith (\i x -> PP.text (show i ++ ") ") <> pPrint x) [1..] (map fst cs)
       is = [ i | (i,(_,correct)) <- zip [1..] cs, correct ]
  return (desc,is)
