{-# LANGUAGE TupleSections #-}
module Test.IOTasks.Trace.Match where

import Test.IOTasks.Trace.NormalizedTrace
import Test.IOTasks.Trace.Class (genProgWrite)
import Test.IOTasks.OutputPattern ((>:))
import qualified Test.IOTasks.OutputPattern as Pattern (text)
import Test.IOTasks.Trace.PrettyPrint

import Text.PrettyPrint hiding ((<>))
import Data.Bifunctor (second)

data MatchResult
  = MatchSuccessful
  | InputMismatch NTraceGeneralized NTracePlain
  | OutputMismatch NTraceGeneralized NTracePlain
  | AlignmentMismatch NTraceGeneralized NTracePlain
  | TerminationMismatch NTraceGeneralized (Maybe NTraceGeneralized) NTracePlain
  deriving (Eq,Show)

isSuccessfulMatch :: MatchResult -> Bool
isSuccessfulMatch MatchSuccessful = True
isSuccessfulMatch _ = False

isInputMismatch :: MatchResult -> Bool
isInputMismatch InputMismatch{} = True
isInputMismatch _ = False

isOutputMismatch :: MatchResult -> Bool
isOutputMismatch OutputMismatch{} = True
isOutputMismatch _ = False

isAlignmentMismatch :: MatchResult -> Bool
isAlignmentMismatch AlignmentMismatch{} = True
isAlignmentMismatch _ = False

isTerminationMismatch :: MatchResult -> Bool
isTerminationMismatch TerminationMismatch{} = True
isTerminationMismatch _ = False

instance Semigroup MatchResult where
  a <> b = case choose a b of
    Left x -> x
    Right x -> x

choose :: MatchResult -> MatchResult -> Either MatchResult MatchResult
choose MatchSuccessful _ = Left MatchSuccessful
choose _ MatchSuccessful = Right MatchSuccessful
choose r _ = Left r

addExpect :: NTraceGeneralized -> (MatchResult,HasConsumed) -> (MatchResult,HasConsumed)
addExpect s' (TerminationMismatch t Nothing s,False) = (TerminationMismatch t (Just s') s,False)
addExpect _ r = r

type HasConsumed = Bool

covers :: NTraceGeneralized -> NTracePlain -> MatchResult
covers s t = fst $ covers' s t
  where
    -- HasConsume tracks if the result has consumed at least one step from t
    covers' :: NTraceGeneralized -> NTracePlain -> (MatchResult, HasConsumed)
    covers' s@(NProgRead i t1) t@(NProgRead j t2)
      | i == j = hasConsumed $ t1 `covers'` t2
      | otherwise = noConsume $ InputMismatch s t

    covers' s@(NProgWrite (Mandatory,is) t1) t@(NProgWrite j t2)
      | any (>: Pattern.text j) is = hasConsumed $ t1 `covers'` t2
      | otherwise = noConsume $ OutputMismatch s t

    covers' (NProgWrite (Optional,is) t1@NProgRead{}) t@(NProgWrite{}) =
      -- avoid the alignment mismatch that would result from trying to skip the optional output
      genProgWrite (Mandatory,is) t1 `covers'` t
    covers' s@(NProgWrite (Optional,is) t1) t =
      let
        (r1,c1) = addExpect s (t1 `covers'` t) -- cannot produce alignment mismatch!
        (r2,c2) = (genProgWrite (Mandatory,is) t1 `covers'` t)
      in case choose r1 r2 of -- This is r1 <> r2 + bookkeeping for the consumption status
        Left _ -> (r1,c1)
        Right _ -> (r2,c2)

    covers' NTerminate NTerminate = (MatchSuccessful,True)
    covers' s@NTerminate t = noConsume $ TerminationMismatch s Nothing t

    covers' NOutOfInputs NOutOfInputs = (MatchSuccessful,True)

    covers' s t = noConsume $ AlignmentMismatch s t

noConsume :: MatchResult -> (MatchResult,HasConsumed)
noConsume = (,False)

hasConsumed :: (MatchResult,HasConsumed) -> (MatchResult,HasConsumed)
hasConsumed = second (const True)

printMatchResult :: (Doc -> Doc -> Doc) -> MatchResult -> Doc
printMatchResult = pPrintMatchResult' False

printMatchResultSimple :: (Doc -> Doc -> Doc) -> MatchResult -> Doc
printMatchResultSimple = pPrintMatchResult' True

pPrintMatchResult' :: Bool -> (Doc -> Doc -> Doc) -> MatchResult -> Doc
pPrintMatchResult' _ _ MatchSuccessful = text "MatchSuccessful"
pPrintMatchResult' simple f (InputMismatch s t) = text "InputMismatch:" $$ nest 2 (reportMismatch simple f s Nothing t)
pPrintMatchResult' simple f (OutputMismatch s t) = text "OutputMismatch:" $$ nest 2 (reportOutputMismatch simple f s t)
pPrintMatchResult' simple f (AlignmentMismatch s t) = text "AlignmentMismatch:" $$ nest 2 (reportMismatch simple f s Nothing t)
pPrintMatchResult' simple f (TerminationMismatch s s' t) = text "TerminationMismatch:" $$ nest 2 (reportMismatch simple f s s' t)

reportMismatch :: Bool -> (Doc -> Doc -> Doc) -> NTraceGeneralized -> Maybe NTraceGeneralized -> NTracePlain -> Doc
reportMismatch simple f s s' t = vcat
  [ text "Expected:"
  , nest 2 (maybe mempty ((`f` text "or") . printTraceHeadHow) s' <+> printTraceHeadHow s)
  , text "Got:"
  , nest 2 (printTraceHeadHow t)
  ]
  where
    printTraceHeadHow :: PrettyPrintTrace t => t -> Doc
    printTraceHeadHow
      | simple = printTraceHeadSimple
      | otherwise = printTraceHead

reportOutputMismatch :: Bool -> (Doc -> Doc -> Doc) -> NTraceGeneralized -> NTracePlain -> Doc
reportOutputMismatch simple f s t = printTraceHeadHow t `f` text "is not covered by" `f` printTraceHeadHow s
  where
    printTraceHeadHow :: PrettyPrintTrace t => t -> Doc
    printTraceHeadHow
      | simple = printTraceHeadSimple
      | otherwise = printTraceHead
