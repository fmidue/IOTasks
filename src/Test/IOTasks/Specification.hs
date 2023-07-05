{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Specification where

import Test.IOTasks.ValueSet
import Test.IOTasks.Term
import Test.IOTasks.Terms (Var (..), varname)
import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern
import Test.IOTasks.Overflow
import Test.IOTasks.ValueMap

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub,intersperse)
import Data.Functor.Identity (runIdentity,Identity(..))
import Data.Bifunctor (first)
import Data.Typeable

import Text.PrettyPrint hiding ((<>))

data Specification where
  ReadInput :: (Typeable a,Read a,Show a) => Var -> ValueSet a -> InputMode -> Specification -> Specification
  WriteOutput :: OptFlag -> Set (OutputPattern 'SpecificationP) -> Specification -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification
  TillE :: Specification -> Specification -> Specification
  E :: Specification

data InputMode = AssumeValid | UntilValid | Abort deriving (Eq,Show)

instance Semigroup Specification where
  s <> Nop = s
  Nop <> s = s
  (ReadInput x vs m s) <> s' = ReadInput x vs m $ s <> s'
  (WriteOutput o t s) <> s' = WriteOutput o t $ s <> s'
  (Branch c l r s) <> s' = Branch c l r $ s <> s'
  TillE bdy s <> s' = TillE bdy $ s <> s'
  E <> _ = E

instance Monoid Specification where
  mempty = nop

readInput :: (Typeable a,Read a,Show a) => Var -> ValueSet a -> InputMode -> Specification
readInput = readInput' where
  readInput' :: forall a. (Typeable a,Read a,Show a) => Var -> ValueSet a -> InputMode -> Specification
  readInput' (Var (x,ty)) vs m
    | ty == typeRep (Proxy @a) = ReadInput (Var (x,ty)) vs m nop
    | otherwise = error "readInput: types of variable and ValueSet do not match"

writeOutput :: [OutputPattern 'SpecificationP] -> Specification
writeOutput ts = WriteOutput Mandatory (Set.fromList ts) nop

writeOptionalOutput :: [OutputPattern 'SpecificationP] -> Specification
writeOptionalOutput ts = WriteOutput Optional (Set.fromList ts) nop

optionalTextOutput :: Specification
optionalTextOutput = writeOptionalOutput [Wildcard]

branch :: Term Bool -> Specification -> Specification -> Specification
branch c t e = Branch c t e nop

nop :: Specification
nop = Nop

tillExit :: Specification -> Specification
tillExit bdy = TillE bdy nop

exit :: Specification
exit = E

until :: Term Bool -> Specification -> Specification
until c bdy = TillE (branch c exit bdy) nop

while :: Term Bool -> Specification -> Specification
while c bdy = TillE (branch c bdy exit) nop

vars :: Specification -> [Var]
vars = nub . go where
  go (ReadInput x _ _ s') = x : go s'
  go (WriteOutput _ _ s') = go s'
  go (Branch _ l r s') = go l ++ go r ++ go s'
  go Nop = []
  go (TillE bdy s') = go bdy ++ go s'
  go E = []

hasIteration :: Specification -> Bool
hasIteration (ReadInput _ _ _ s') = hasIteration s'
hasIteration (WriteOutput _ _ s') = hasIteration s'
hasIteration (Branch _ l r s') = hasIteration l || hasIteration r || hasIteration s'
hasIteration TillE{} = True
hasIteration Nop = False
hasIteration E = False

runSpecification :: [String] -> Specification -> (AbstractTrace,OverflowWarning)
runSpecification = runSpecification' True

type AddLinebreaks = Bool

runSpecification' :: AddLinebreaks -> [String] -> Specification -> (AbstractTrace,OverflowWarning)
runSpecification' addLinebreaks inputs spec =
  sem
    (\(e,ins) x (vs :: ValueSet v) mode ->
      case ins of
        [] -> NoRec (outOfInputs,NoOverflow)
        (i:is)
          | vs `containsValue` readValue i -> RecSub i id (insertValue (wrapValue $ readValue @v i) x e,is)
          | otherwise -> case mode of
              AssumeValid -> error $ "invalid value: " ++ i ++ " is not an element of " ++ printValueSet vs
              UntilValid ->  RecSame i (first (progWrite Optional (Set.singleton Wildcard) <>)) (e,is)
              Abort -> NoRec (foldr ((<>) . progRead) (((<>) . progRead) '\n' $ progWrite Optional (Set.singleton Wildcard)) i,NoOverflow)
    )
    (\case
      NoRec r -> r
      RecSub i () (t',w) -> (foldr ((<>) . progRead) (((<>) . progRead) '\n' t')  i,w)
      RecSame i () (t',w) -> (foldr ((<>) . progRead) (((<>) . progRead) '\n' t') i,w)
      RecBoth{} -> error "runSpecification: impossible"
    )
    (\(e,_) o ts (t',ww) ->
      let (warn,os) = Set.foldr (\t (w,s) -> let (w',p) = evalPattern e t in (w <> w', Set.insert p s)) (NoOverflow, mempty) ts
          os' = if addLinebreaks then os `Set.union` Set.map (<> Text "\n") os else os
      in (progWrite o os' <> t', warn <> ww)
    )
    (\(e,_) c (l,wl) (r,wr) ->
      let (w,b) = eval c e
      in if b then (l,wl <> w) else (r,wr <> w))
    (const id)
    (terminate,NoOverflow)
    (emptyValueMap $ vars spec,inputs)
    spec

data RecStruct p x a r = NoRec r | RecSub p x a | RecSame p x a | RecBoth p x a a

sem :: forall st p a.
  (forall v. (Typeable v,Read v,Show v) => st -> Var -> ValueSet v -> InputMode -> RecStruct p (a->a) st a) -> (RecStruct p () a a -> a) ->
  (st -> OptFlag -> Set (OutputPattern 'SpecificationP) -> a -> a) ->
  (st -> Term Bool -> a -> a -> a) ->
  (Action -> a -> a) ->
  a ->
  st -> Specification -> a
sem f f' g h i z st s = runIdentity $ semM
  (\a b c d -> Identity $ f a b c d)
  (Identity . f')
  (\a b c -> Identity . g a b c . runIdentity)
  (\a b c d -> Identity $ h a b (runIdentity c) (runIdentity d))
  (\a b -> Identity $ i a (runIdentity b))
  (pure z)
  st
  s

semM :: forall m st p a. Monad m =>
  (forall v. (Typeable v,Read v,Show v) => st -> Var -> ValueSet v -> InputMode -> m (RecStruct p (a->a) st a)) -> (RecStruct p () a a -> m a) ->
  (st -> OptFlag -> Set (OutputPattern 'SpecificationP) -> m a -> m a) ->
  (st -> Term Bool -> m a -> m a -> m a) ->
  (Action -> m a -> m a) ->
  m a ->
  st -> Specification -> m a
semM f f' g h i z s_I spec = sem' s_I spec k_I where
  sem' :: st -> Specification -> (Action ->  st -> m a) -> m a
  sem' st s@(ReadInput x vs mode s') k =
    do
      let mStruct = f st x vs mode
      struct <- mStruct
      f' =<< case struct of
        NoRec r -> pure $ NoRec r
        RecSub p r st' -> RecSub p () . r <$> sem' st' s' k
        RecSame p r st' -> RecSame p () . r <$> sem' st' s k
        RecBoth p r st' st'' -> RecBoth p () . r <$> sem' st' s' k <*> sem' st'' s k
  sem' st (WriteOutput o ts s') k = g st o ts $ sem' st s' k
  sem' st (Branch c l r s') k = h st c (sem' st (l <> s') k) (sem' st (r <> s') k)
  sem' st (TillE s s') k = sem' st s k'
    where
      k' End st = i End $ sem' st s k'
      k' Exit st = i Exit $ sem' st s' k
  sem' st Nop k = k End st
  sem' st E k = k Exit st

  k_I :: Action ->  st -> m a
  k_I End _ = z
  k_I Exit _ = error "ill-formed specification: exit marker at top-level"

data Action = End | Exit

pPrintSpecification :: Specification -> Doc
pPrintSpecification (ReadInput x vs m s) = text (concat ["[▷ ",varname x," ∈ ", printValueSet vs, "]",printInputMode m]) $$ pPrintSpecification s
pPrintSpecification (WriteOutput opt ts s) = text (concat $ ["[{",if opt == Optional then "𝜀," else ""] ++ intersperse "," (map printPatternSimple (Set.toList ts)) ++ ["}▷ ]"]) $$ pPrintSpecification s
pPrintSpecification (Branch c t e s) = text (concat ["[",printTerm c,"]⇒ ("]) <> pPrintSpecification t <> text "△ " <> pPrintSpecification e <> text ")" $$ pPrintSpecification s
pPrintSpecification Nop = text "0"
pPrintSpecification (TillE bdy s) = text "(" <> pPrintSpecification bdy <> text ")🠒ᴱ" $$ pPrintSpecification s
pPrintSpecification E = text "E"

printInputMode :: InputMode -> String
printInputMode AssumeValid = ""
printInputMode UntilValid = "↻"
printInputMode Abort = "↯"

accept :: Specification -> Trace -> Bool
accept s_ t_ = accept' s_ k_I t_ d_I
  where
    accept' :: Specification -> (Action -> Trace -> ValueMap -> Bool) -> Trace -> ValueMap -> Bool
    accept' (ReadInput x (ty :: ValueSet a) AssumeValid s') k t d = case t of
      ProgReadString v t' | ty `containsValue` val -> accept' s' k t' (insertValue (wrapValue val) x d)
                          where val = readValue @a v
      _ -> False
    accept' (ReadInput x (ty :: ValueSet a) Abort s') k t d = case t of
      ProgReadString v t'| ty `containsValue` val -> accept' s' k t' (insertValue (wrapValue val) x d)
                         where val = readValue @a v
      ProgReadString v Terminate | not (ty `containsValue` readValue v) -> True
      _ -> False
    accept' s@(ReadInput x (ty :: ValueSet a) UntilValid s') k t d = case t of
      ProgReadString v t'
        | ty `containsValue` val -> accept' s' k t' (insertValue (wrapValue val) x d)
        | not (ty `containsValue` val) -> accept' s k t d
        where val = readValue @a v
      _ -> False
    accept' (WriteOutput Optional os s') k t d = accept' (WriteOutput Mandatory os s') k t d || accept' s' k t d
    accept' (WriteOutput Mandatory os s') k t d =  case t of
      ProgWrite Mandatory vs t' | vs `Set.isSubsetOf` Set.map (snd . evalPattern d) os -> accept' s' k t' d
      _ -> False
    accept' (Branch c s1 s2 s') k t d
      | snd (eval c d) = accept' (s1 <> s') k t d
      | otherwise = accept' (s2 <> s') k t d
    accept' (TillE s s') k t d = accept' s k' t d
      where
        k' End = accept' s k'
        k' Exit = accept' s' k
    accept' E k t d = k Exit t d
    accept' Nop k t d = k End t d

    k_I :: Action -> Trace -> ValueMap -> Bool
    k_I End Terminate _ = True
    k_I End _ _ = False
    k_I Exit _ _ = error "ill-formed specification: exit marker at top-level"
    d_I :: ValueMap
    d_I = emptyValueMap $ vars s_
