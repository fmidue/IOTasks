module Test.IOTasks.TaskGeneration.Task where

import Data.Char (isSpace)

import Test.QuickCheck

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.PrettyPrint.HughesPJClass as PP

import Text.Parsec
import Text.Parsec.String (Parser)

import Test.IOTasks hiding (putStrLn, getLine)
import Test.IOTasks.Trace
import Test.IOTasks.SpecGen

import Test.IOTasks.CodeGeneration

-- Internal API --

-- tasks over solution type s
data Task s = Task
  { generator :: Gen (Specification SpecTerm) -- generator for the underlying specification
  , body :: Specification SpecTerm -> TaskBody s -- the actual task, depending on the generated specifciation
  }

data TaskBody s = TaskBody
  { question :: Description -- a (verbal) description of the task
  , requires :: Require s -- the decider for a correct solution
  }

type Description = PP.Doc
type Require s = s -> Property

taskRunnerIO :: IO a -> Task a -> IO ()
taskRunnerIO getAnswer t = do
  spec <- generate $ generator t
  let TaskBody desc req = body t spec
  putStrLn $ PP.render desc
  quickCheck . req =<< getAnswer

-- external API --
type Program = IOrep ()

forUnknownSpec :: Gen (Specification SpecTerm) -> (Specification SpecTerm -> TaskBody s) -> Task s
forUnknownSpec = Task

imperativeProgram :: Specification SpecTerm -> Description
imperativeProgram = pseudoCode . programIR

haskellProgram :: Specification SpecTerm -> Description
haskellProgram = haskellCode . programIR

behavior :: Specification SpecTerm -> Require Program
behavior = flip fulfills

sampleTrace :: Specification SpecTerm -> Require (Trace String)
sampleTrace s t = property $ accept s t

solveWith :: Description -> Require s -> TaskBody s
solveWith = TaskBody

-- example task
task :: Task (Trace String)
task = forUnknownSpec simpleSpec $ \s ->
  (PP.text "Give an example interaction for the following Haskell program:"
    PP.$$ haskellProgram s)
  `solveWith` sampleTrace s

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
