{-# LANGUAGE FlexibleContexts #-}
module Test.IOTasks.TaskGeneration.Task where

import Data.Char (isSpace)

import Test.QuickCheck

import qualified Text.PrettyPrint.HughesPJ as PP

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Term.Class
import Data.Term.AST

import Test.IOTasks hiding (putStrLn, getLine)
import Test.IOTasks.Trace
import Test.IOTasks.SpecGen
import Test.IOTasks.CodeGeneration

-- Internal API --

-- tasks over solution type s
data Task s = Task
  { generator :: Gen (Specification SpecTerm) -- generator for the underlying specification
  , body :: Specification SpecTerm -> Gen (TaskInstance s) -- the actual task generator, depending on the generated specifciation
  }

data TaskInstance s = TaskInstance
  { question :: Description -- a (verbal) description of the task
  , requires :: Require s -- the decider for a correct solution
  }

type Description = PP.Doc
type Require s = s -> Property

taskRunnerIO :: IO a -> Task a -> IO ()
taskRunnerIO getAnswer t = do
  TaskInstance desc req <- generateTaskInstance t
  putStrLn $ PP.render desc
  quickCheck . req =<< getAnswer

showTaskInstance :: Task a -> IO ()
showTaskInstance t = do
  i <- generateTaskInstance t
  putStrLn . PP.render $ question i

generateTaskInstance :: Task a -> IO (TaskInstance a)
generateTaskInstance t =
  generate $ do
    spec <- generator t
    body t spec

-- external API --
type Program = IOrep ()

forUnknownSpec :: Gen (Specification SpecTerm) -> (Specification SpecTerm -> Gen (TaskInstance s)) -> Task s
forUnknownSpec = Task

imperativeProgram :: Specification SpecTerm -> Description
imperativeProgram = pseudoCode . programIR

specProgram :: (SynTerm t (AST Varname), VarListTerm t Varname) => Specification t -> Gen IRProgram
specProgram p =
  let ps = fst $ runFreshVarM (programVariants =<< programIR' p) emptyVarInfo
  in elements ps

behavior :: Specification SpecTerm -> Require Program
behavior = flip fulfills

sampleTrace :: Specification SpecTerm -> Require (Trace String)
sampleTrace s t = property $ accept s t

compilingProgram :: Require String
compilingProgram _ = property True -- TODO: implement, or let submission platform check this.

solveWith :: Description -> Require s -> TaskInstance s
solveWith = TaskInstance

-- example task
task :: Task Program
task = forUnknownSpec simpleSpec $ \s -> do
  prog <- pseudoCode <$> specProgram s
  return $
    ( PP.text "Re-implement the following program in Haskell:"
      PP.$$ prog
    ) `solveWith` behavior s

task1 :: Task (Trace String)
task1 = forUnknownSpec simpleSpec $ \s -> do
  prog <- haskellCode <$> specProgram s
  return $
    ( PP.text "Give an example interaction for the following Haskell program:"
      PP.$$ prog
    ) `solveWith` sampleTrace s

type Code = String

task2 :: Task Code
task2 = forUnknownSpec simpleSpec $ \s -> do
  prog <- haskellWithReadWriteHoles <$> specProgram s
  return $
    ( PP.text "Complete the following template into a syntactically correct program"
      PP.$$ PP.text "(replace the ??? with calls to readLn and print)"
      PP.$$ prog
    ) `solveWith` compilingProgram

readTrace :: IO (Trace String)
readTrace = do
  inp <- getLine
  let tr = parse traceParser "<string>" inp
  either (error . show) return tr

traceParser :: Parser (Trace String)
traceParser = chainr (parseStep '?' ProgRead <|> parseStep '!' ProgWrite ) ((<>) <$ satisfy isSpace) Stop
  where
    parseStep :: Char -> (String -> Trace String -> Trace String) -> Parser (Trace String)
    parseStep mark cnstr = do
      _ <- char mark
      i <- many alphaNum
      pure $ cnstr i Stop