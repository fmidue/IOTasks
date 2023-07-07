{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.IOTasks.Task where

import Test.QuickCheck

import Control.Arrow

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

instance Contravariant Require where
  contramap f (RequireProp r) = RequireProp (r . f)
  contramap f (RequirePure r) = RequirePure (r . f)
  contramap f (AndR r s) = AndR (contramap f r) (contramap f s)
  contramap f (RequireAfterIO r k) = RequireAfterIO r (k . f)

instance Divisible Require where
  divide f r s = AndR (contramap (fst . f) r) (contramap (snd . f) s)
  conquer = RequirePure . const $ True

type Description = PP.Doc

data TaskInstance s = TaskInstance
    { question :: Description
    , given :: Maybe s
    , requires :: Require s
    }

data Require s where
  RequireProp :: (s -> Property) -> Require s
  RequirePure :: (s -> Bool) -> Require s
  AndR :: Require s -> Require s -> Require s
  RequireAfterIO :: Require s' -> (s -> IO (Maybe s')) ->  Require s

check :: Require s -> s -> IO Bool
check (RequireProp p) s = fmap isSuccess . quickCheckResult $ p s
check (RequirePure p) s = pure $ p s
check (AndR r t) s = (&&) <$> check r s <*> check t s
check (RequireAfterIO p f) s = f s >>= maybe (pure False) (check p)

checkPure :: Require s -> Maybe (s -> Bool)
checkPure (RequireProp _) = Nothing
checkPure (RequireAfterIO _ _) = Nothing
checkPure (RequirePure p) = Just p
checkPure (AndR r t) = (\p q s -> p s && q s) <$> checkPure r <*> checkPure t

(/\) :: Require s -> Require s -> Require s
AndR r s /\ t = r /\ (s /\ t)
RequirePure p /\ RequireProp q = requireProp q /\ requirePure p
RequireProp p /\ RequireProp q = requireProp $ \s -> p s .&&. q s
p@(RequireProp _) /\ (AndR q@(RequireProp _) r) = (p /\ q) /\ r
r /\ s = AndR r s

requireProp :: (s -> Property) -> Require s
requireProp = RequireProp

requirePure :: (s -> Bool) -> Require s
requirePure = RequirePure

after :: Require s' -> (s -> IO (Maybe s')) -> Require s
after = RequireAfterIO

class Matches s where
  -- should define a partial order on s
  matches :: s -> s -> Bool

mustMatch :: Matches s => s -> Require s
mustMatch = requirePure . matches

exactAnswer :: (Eq a, Show a) => a -> Require a
exactAnswer x = requireProp $ \s -> s === x

data TaskDesign s = forall p. TaskDesign
  { parameter :: Gen p
  , instantiate :: p -> TaskInstance s
  }

runTaskIO :: Show s => TaskDesign s -> IO s -> IO ()
runTaskIO task getAnswer = do
  TaskInstance q g req <- generateTaskInstance task
  putStrLn $ PP.render q
  case g of
    Just s -> putStrLn $ unlines ["","solution template:",show s]
    Nothing -> pure ()
  s <- getAnswer
  isCorrect <- check req s
  if isCorrect
    then putStrLn "Correct solution"
    else putStrLn "Incorrect solution"

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

-- solveWith :: Description -> Require s -> TaskInstance s
-- solveWith = TaskInstance

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
  let  desc = PP.vcat $ zipWith (\i x -> PP.text $ show i ++ ") " ++ show x) [(1 :: Int)..] (map fst cs)
       is = [ i | (i,(_,correct)) <- zip [1..] cs, correct ]
  return (desc,is)

multipleChoicePP :: Pretty a => Int -> [a] -> [a] -> Gen (Description, [Int])
multipleChoicePP n rs ws = do
  let  rs' = map (,True) rs
       ws' = map (,False) ws
  cs <- take n <$> shuffle (rs' ++ ws')
  let  desc = PP.vcat $ zipWith (\i x -> PP.text (show i ++ ") ") <> pPrint x) [(1 :: Int)..] (map fst cs)
       is = [ i | (i,(_,correct)) <- zip [1..] cs, correct ]
  return (desc,is)
