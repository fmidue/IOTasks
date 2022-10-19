{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module IOTasks.Specification where

import IOTasks.ValueSet
import IOTasks.Term
import IOTasks.Terms (Varname)
import IOTasks.Trace
import IOTasks.OutputPattern
import IOTasks.Overflow

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import qualified Data.Map as Map
import Data.Functor.Identity (runIdentity,Identity(..))

data Specification where
  ReadInput :: Varname -> ValueSet -> InputMode -> Specification -> Specification
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

readInput :: Varname -> ValueSet -> InputMode -> Specification
readInput x vs m = ReadInput x vs m nop

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

vars :: Specification -> [Varname]
vars = nub . go where
  go (ReadInput x _ _ s') = x : go s'
  go (WriteOutput _ _ s') = go s'
  go (Branch _ l r s') = go l ++ go r ++ go s'
  go Nop = []
  go (TillE bdy s') = go bdy ++ go s'
  go E = []

runSpecification :: [String] -> Specification -> (Trace,OverflowWarning)
runSpecification inputs spec =
  sem
    (\(e,ins) x vs mode ->
      case ins of
        [] -> NoRec (OutOfInputs,NoOverflow)
        ((i,n):is)
          | vs `containsValue` read i -> RecSub i (Map.update (\xs -> Just $ (read i,n):xs) x e,is)
          | otherwise -> case mode of
              AssumeValid -> error $ "invalid value: " ++ i ++ " is not an element of " ++ printValueSet vs
              UntilValid -> RecSame i (e,is)
              Abort -> NoRec (foldr ProgRead (ProgRead '\n' Terminate) i,NoOverflow)
    )
    (\case
      NoRec r -> r
      RecSub i (t',w) -> (foldr ProgRead (ProgRead '\n' t') i,w)
      RecSame i (t',w) -> (foldr ProgRead (ProgRead '\n' t') i,w)
      RecBoth{} -> error "runSpecification: impossible"
    )
    (\(e,_) o ts (t',ww) ->
      let (warn,os) = Set.foldr (\t (w,s) -> let (w',p) = evalPattern e t in (w <> w', Set.insert p s)) (NoOverflow, mempty) ts
      in (progWrite o (os `Set.union` Set.map (<> Text "\n") os) <> t', warn <> ww)
    )
    (\(e,_) c (l,wl) (r,wr) ->
      let (w,b) = eval c e
      in if b then (l,wl <> w) else (r,wr <> w))
    (Terminate,NoOverflow)
    (Map.fromList ((,[]) <$> vars spec),inputs `zip` [1..])
    spec

data RecStruct p a r = NoRec r | RecSub p a | RecSame p a | RecBoth p a a

sem :: forall st p a.
  (st -> Varname -> ValueSet -> InputMode -> RecStruct p st a) -> (RecStruct p a a -> a) ->
  (st -> OptFlag -> Set (OutputPattern 'SpecificationP) -> a -> a) ->
  (st -> Term Bool -> a -> a -> a) ->
  a ->
  st -> Specification -> a
sem f f' g h z st s = runIdentity $ semM
  (\a b c d -> Identity $ f a b c d)
  (Identity . f')
  (\a b c -> Identity . g a b c . runIdentity)
  (\a b c d -> Identity $ h a b (runIdentity c) (runIdentity d))
  (pure z)
  st
  s

semM :: forall m st p a. Monad m =>
  (st -> Varname -> ValueSet -> InputMode -> m (RecStruct p st a)) -> (RecStruct p a a -> m a) ->
  (st -> OptFlag -> Set (OutputPattern 'SpecificationP) -> m a -> m a) ->
  (st -> Term Bool -> m a -> m a -> m a) ->
  m a ->
  st -> Specification -> m a
semM f f' g h z s_I spec = sem' s_I spec k_I where
  sem' :: st -> Specification -> (T ->  st -> m a) -> m a
  sem' st s@(ReadInput x vs mode s') k =
    do
      let mStruct = f st x vs mode
      struct <- mStruct
      f' =<< case struct of
        NoRec r -> pure $ NoRec r
        RecSub p st' -> RecSub p <$> sem' st' s' k
        RecSame p st' -> RecSame p <$> sem' st' s k
        RecBoth p st' st'' -> RecBoth p <$> sem' st' s' k <*> sem' st'' s k
  sem' st (WriteOutput o ts s') k = g st o ts $ sem' st s' k
  sem' st (Branch c l r s') k = h st c (sem' st (l <> s') k) (sem' st (r <> s') k)
  sem' st (TillE s s') k = sem' st s k'
    where
      k' End st = sem' st s k'
      k' Exit st = sem' st s' k
  sem' st Nop k = k End st
  sem' st E k = k Exit st

  k_I :: T ->  st -> m a
  k_I End _ = z
  k_I Exit _ = error "ill-formed specification: exit marker at top-level"

data T = End | Exit
