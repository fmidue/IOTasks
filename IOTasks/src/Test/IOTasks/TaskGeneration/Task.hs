{-# LANGUAGE TupleSections #-}
module Test.IOTasks.TaskGeneration.Task where

import Test.QuickCheck

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

instance Contravariant (TaskTemplate s) where
  contramap f (TaskTemplate g fb) = TaskTemplate g (fmap (contramap f) . fb)

type Description = PP.Doc

data TaskInstance s = TaskInstance
    { question :: Description
    , requires :: Require s
    }

newtype Require s = Require { check :: s -> Property}

exactAnswer :: (Eq a, Show a) => a -> Require a
exactAnswer x = Require $ \s -> s === x

data TaskTemplate p s = TaskTemplate
  { parameter :: Gen p
  , inst :: p -> Gen (TaskInstance s)
  }

runTaskIO :: TaskTemplate p s -> IO s -> IO ()
runTaskIO task getAnswer = do
  TaskInstance q req <- generateTaskInstance task
  putStrLn $ PP.render q
  s <- getAnswer
  quickCheck $ check req s

showTaskInstance :: TaskTemplate p s -> IO ()
showTaskInstance t = do
  i <- generateTaskInstance t
  putStrLn . PP.render $ question i

generateTaskInstance :: TaskTemplate p s -> IO (TaskInstance s)
generateTaskInstance task =
  generate $ do
    p <- parameter task
    inst task p

forFixed :: p -> (p -> Gen (TaskInstance s)) -> TaskTemplate p s
forFixed p = TaskTemplate (pure p)

forUnknown :: Gen p -> (p -> Gen (TaskInstance s)) -> TaskTemplate p s
forUnknown = TaskTemplate

solveWith :: Description -> Require s -> TaskInstance s
solveWith = TaskInstance

(/\) :: Require a -> Require b -> Require (a,b)
(/\) = divided

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
