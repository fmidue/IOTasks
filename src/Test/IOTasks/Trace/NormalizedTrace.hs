{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Test.IOTasks.Trace.NormalizedTrace (
  NTracePlain, NTraceGeneralized,
  NormalizedTrace,
  OptFlag(..),
  isTerminatingNTrace,
  pattern NProgRead, pattern NProgWrite, pattern NTerminate, pattern NOutOfInputs,
  printNTracePlain, printNTracePlain', printNTracePlainHead,
  printNTraceGeneralized, printNTraceGeneralized', printNTraceGeneralizedSimple, printNTraceGeneralizedSimple',
  printNTraceGeneralizedHead, printNTraceGeneralizedHeadSimple,
  ) where

import Test.IOTasks.Trace.Class
import Test.IOTasks.OutputPattern

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (second)

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Data.Monoid (Endo(..))

data NormalizedTrace a
  = NTrace (Maybe (NTraceWrite a)) (NTraceInner a) (Maybe NTraceRead) NTraceFinal
  deriving Eq

data NTraceInner a
  = ReadAndWrite NTraceRead (NTraceWrite a) (NTraceInner a)
  | EmptyNTrace
  deriving Eq

newtype NTraceRead = NTraceRead String
  deriving Eq

newtype NTraceWrite a = NTraceWrite a
  deriving Eq

data NTraceFinal = Terminate | OutOfInputs
  deriving Eq

data OptFlag = Optional | Mandatory
  deriving (Eq, Ord, Show)

foldrNTrace :: (NTraceFinal -> a) -> (NTraceRead -> a -> a) -> (NTraceWrite b -> a -> a) -> NormalizedTrace b -> a
foldrNTrace z f g (NTrace w t r fin) = maybe id g w $ foldInner t $ maybe id f r $ z fin
  where
    foldInner EmptyNTrace = id
    foldInner (ReadAndWrite r' w' t') = f r' . g w' . foldInner t'

instance Show a => Show (NormalizedTrace a) where
  show = foldrNTrace showFinal (addDollar showRead) (addDollar showWrite)
    where
      showFinal :: NTraceFinal -> String
      showFinal Terminate = "terminate"
      showFinal OutOfInputs = "outOfInputs"
      addDollar :: (a -> String) -> a -> String -> String
      addDollar f x s@"terminate" = f x <> " " <> s
      addDollar f x s@"outOfInputs" = f x <> " " <> s
      addDollar f x s = f x <> " $ " <> s
      showRead :: NTraceRead -> String
      showRead (NTraceRead s) = unwords ["progReadLine",show s]
      showWrite :: Show a => NTraceWrite a -> String
      showWrite (NTraceWrite s) = unwords ["progWriteStr",show s]

instance Semigroup (NormalizedTrace String) where
  x@(NTrace _ _ _ OutOfInputs) <> _ = x
  x <> y = foldrNTrace (const y) (\(NTraceRead r) -> progReadLine r) (\(NTraceWrite s)-> progWriteLine s) x

instance Monoid (NormalizedTrace String) where
  mempty = terminate

instance Trace (NormalizedTrace String) where
  progReadChar = progReadChar'
  progWriteStr = progWrite' (<>)
  terminate = terminate'
  outOfInputs = outOfInputs'
  inputSequence = inputSequence'

instance Semigroup (NormalizedTrace (OptFlag, Set (OutputPattern 'TraceP))) where
  x@(NTrace _ _ _ OutOfInputs) <> _ = x
  x <> y = foldrNTrace (const y) (\(NTraceRead r) -> progReadLine r) (\(NTraceWrite s)-> genProgWriteLine s) x

instance Monoid (NormalizedTrace (OptFlag, Set (OutputPattern 'TraceP))) where
  mempty = terminate

instance Trace (NormalizedTrace (OptFlag, Set (OutputPattern 'TraceP))) where
  progReadChar = progReadChar'
  -- progReadLine = progReadLine'
  progWriteStr = genProgWrite . liftString
  progWriteLine = genProgWriteLine . liftString
  terminate = terminate'
  outOfInputs = outOfInputs'
  inputSequence = inputSequence'

liftString :: String -> (OptFlag, Set (OutputPattern 'TraceP))
liftString s = (Mandatory, Set.singleton $ text s)

combineOutputs :: (OptFlag, Set (OutputPattern 'TraceP)) -> (OptFlag, Set (OutputPattern 'TraceP)) -> (OptFlag, Set (OutputPattern 'TraceP))
combineOutputs (o1,ts1) (o2,ts2) = (max o1 o2, ts)
  where
    ts = Set.unions $
      [ ts1 | o2 == Optional ] ++
      [ ts2 | o1 == Optional ] ++
      [Set.map (uncurry (<>)) $ Set.cartesianProduct ts1 ts2]

instance GeneralizedTrace NormalizedTrace (OptFlag, Set (OutputPattern 'TraceP)) where
  genProgWrite = progWrite' combineOutputs
  genProgWriteLine = progWrite' combineOutputs . second (Set.map (<> text "\n"))

progReadChar' :: Char -> NormalizedTrace a -> NormalizedTrace a
progReadChar' c (NTrace Nothing EmptyNTrace Nothing f) = NTrace Nothing EmptyNTrace (Just $ NTraceRead [c]) f
progReadChar' c (NTrace Nothing EmptyNTrace (Just (NTraceRead s')) f) = NTrace Nothing EmptyNTrace (Just $ NTraceRead $ c:s') f
progReadChar' c (NTrace Nothing (ReadAndWrite (NTraceRead s') w t) r f) = NTrace Nothing (ReadAndWrite (NTraceRead $ c:s') w t) r f
progReadChar' c (NTrace (Just w) t r f) = NTrace Nothing (ReadAndWrite (NTraceRead [c]) w t) r f

progWrite' :: (a -> a -> a) -> a -> NormalizedTrace a -> NormalizedTrace a
progWrite' combine s (NTrace (Just (NTraceWrite s')) t r f) = NTrace (Just $ NTraceWrite $ s `combine` s') t r f
progWrite' _ s (NTrace Nothing t r f) = NTrace (Just $ NTraceWrite s) t r f

terminate' :: NormalizedTrace a
terminate' = NTrace Nothing EmptyNTrace Nothing Terminate

outOfInputs' :: NormalizedTrace a
outOfInputs' = NTrace Nothing EmptyNTrace Nothing OutOfInputs

inputSequence' :: NormalizedTrace a -> [String]
inputSequence' (NTrace _ EmptyNTrace Nothing _) = []
inputSequence' (NTrace _ EmptyNTrace (Just (NTraceRead s)) _) = lines s
inputSequence' (NTrace w (ReadAndWrite (NTraceRead s) _ t) r f) = lines s ++ inputSequence' (NTrace w t r f)

isTerminatingNTrace :: NormalizedTrace a -> Bool
isTerminatingNTrace (NTrace _ _ _ Terminate) = True
isTerminatingNTrace (NTrace _ _ _ OutOfInputs) = False

type NTracePlain = NormalizedTrace String

type NTraceGeneralized = NormalizedTrace (OptFlag,Set (OutputPattern 'TraceP))

{-# COMPLETE NProgRead, NProgWrite, NTerminate, NOutOfInputs #-}
pattern NProgRead :: String -> NormalizedTrace a -> NormalizedTrace a
pattern NProgRead s t' <- (extractRead -> Just (s,t'))

extractRead :: NormalizedTrace a -> Maybe (String,NormalizedTrace a)
extractRead (NTrace Nothing EmptyNTrace (Just (NTraceRead s)) fin) =
  case lines s of
    [s] -> Just (s,NTrace Nothing EmptyNTrace Nothing fin)
    (s:xs) -> Just (s,NTrace Nothing EmptyNTrace (Just (NTraceRead $ unlines xs)) fin)
    [] -> error "extractRead: impossible"
extractRead (NTrace Nothing (ReadAndWrite (NTraceRead s) w t) r fin) =
  case lines s of
    [s] -> Just (s,NTrace (Just w) t r fin)
    (s:xs) -> Just (s,NTrace Nothing (ReadAndWrite (NTraceRead $ unlines xs) w t) r fin)
    [] -> error "extractRead: impossible"
extractRead _ = Nothing

pattern NProgWrite :: a -> NormalizedTrace a -> NormalizedTrace a
pattern NProgWrite x t' <- (extractWrite -> Just (x,t'))

extractWrite :: NormalizedTrace a -> Maybe (a,NormalizedTrace a)
extractWrite (NTrace (Just (NTraceWrite x)) t r fin) = Just (x,NTrace Nothing t r fin)
extractWrite _ = Nothing

pattern NTerminate :: NormalizedTrace a
pattern NTerminate <- TraceEnd Terminate

pattern NOutOfInputs :: NormalizedTrace a
pattern NOutOfInputs <- TraceEnd OutOfInputs

pattern TraceEnd :: NTraceFinal -> NormalizedTrace a
pattern TraceEnd fin <- NTrace Nothing EmptyNTrace Nothing fin

printNTracePlain :: NTracePlain -> Doc
printNTracePlain = printNTracePlain' (PP.<+>)

-- | like 'printNTracePlain' but trace steps are combined with the user supplied function
printNTracePlain' :: (Doc -> Doc -> Doc) -> NTracePlain -> Doc
printNTracePlain' = showDeep False (\s -> "!" <> PP.text (show s))

printNTracePlainHead :: NTracePlain -> Doc
printNTracePlainHead = printNTracePlain' const

printNTraceGeneralized :: NTraceGeneralized -> Doc
printNTraceGeneralized = printNTraceGeneralized' (PP.<+>)

-- | like 'printNTraceGeneralized' but trace steps are combined with the user supplied function
printNTraceGeneralized' :: (Doc -> Doc -> Doc) -> NTraceGeneralized -> Doc
printNTraceGeneralized' =
  showDeep False
    (\case
      (Optional,ts) -> "(!{"<> printSet ts <> "})"
      (Mandatory,ts) -> "!{"<> printSet ts <> "}"
    )
    where printSet ts = PP.doubleQuotes $ PP.hcat (PP.punctuate "," (PP.text . showPattern <$> Set.toList ts))

printNTraceGeneralizedSimple :: NTraceGeneralized -> Doc
printNTraceGeneralizedSimple = printNTraceGeneralizedSimple' (PP.<+>)

-- | like 'printNTraceGeneralizedSimple' but trace steps are combined with the user supplied function
printNTraceGeneralizedSimple' :: (Doc -> Doc -> Doc) -> NTraceGeneralized -> Doc
printNTraceGeneralizedSimple' =
  showDeep True
    (\case
      (Optional,_) -> mempty -- omit optional outputs in simplified version
      (Mandatory,ts) -> "!\""<> head (PP.text . showPatternSimple <$> Set.toList ts) <> "\""
    )

printNTraceGeneralizedHead :: NTraceGeneralized -> Doc
printNTraceGeneralizedHead = printNTraceGeneralized' const

printNTraceGeneralizedHeadSimple :: NTraceGeneralized -> Doc
printNTraceGeneralizedHeadSimple = printNTraceGeneralizedSimple' const

showDeep :: Bool -> (a -> Doc) -> (Doc -> Doc -> Doc) -> NormalizedTrace a -> Doc
showDeep simpleRead printA f = foldrNTrace
  (\case
    Terminate -> "stop"
    OutOfInputs -> "excess-input-command"
  )
  (\(NTraceRead s) ->
    let ls = lines s
    in appEndo $ foldMap (\l -> Endo $ f ("?" <> PP.text (show (l <> if simpleRead then "" else "\n")))) ls
  )
  (\(NTraceWrite x) -> f $ printA x
  )
