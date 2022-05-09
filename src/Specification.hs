{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Specification where

import ValueSet
import Term
import Trace
import OutputPattern

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

data InputMode = AssumeValid | UntilValid deriving (Eq,Show)

instance Semigroup Specification where
  s <> Nop = s
  Nop <> s = s
  (ReadInput x vs m s) <> s' = ReadInput x vs m $ s <> s'
  (WriteOutput o t s) <> s' = WriteOutput o t $ s <> s'
  (Branch c l r s) <> s' = Branch c l r $ s <> s'
  TillE bdy s <> s' = TillE bdy $ s <> s'
  E <> _ = E

instance Monoid Specification where
  mempty = Nop

readInput :: Varname -> ValueSet -> InputMode -> Specification
readInput x vs m = ReadInput x vs m nop

writeOutput :: [OutputPattern 'SpecificationP] -> Specification
writeOutput ts = WriteOutput Mandatory (Set.fromList ts) nop

writeOptionalOutput :: [OutputPattern 'SpecificationP] -> Specification
writeOptionalOutput ts = WriteOutput Optional (Set.fromList ts) nop

branch :: Term Bool -> Specification -> Specification -> Specification
branch c t e = Branch c t e nop

nop :: Specification
nop = Nop

until :: Term Bool -> Specification -> Specification
until c bdy = TillE (branch c E bdy) nop

vars :: Specification -> [Varname]
vars = nub . go where
  go (ReadInput x _ _ s') = x : go s'
  go (WriteOutput _ _ s') = go s'
  go (Branch _ l r s') = go l ++ go r ++ go s'
  go Nop = []
  go (TillE bdy s') = go bdy ++ go s'
  go E = []

runSpecification :: [String] -> Specification -> Trace
runSpecification inputs spec =
  sem
    (\(e,ins) x vs mode ->
      case ins of
        [] -> NoRec
        (i:is)
          | vs `containsValue` read i -> RecSub i (Map.update (\xs -> Just $ read i:xs) x e,is)
          | otherwise -> case mode of
              AssumeValid -> error "invalid value"
              UntilValid -> RecSame i (e,is)
    )
    (\case
      NoRec -> OutOfInputs
      RecSub i t' -> foldr ProgRead (ProgRead '\n' t') i
      RecSame i t' -> foldr ProgRead (ProgRead '\n' t') i
      RecBoth{} -> error "runSpecification: impossible"
    )
    (\(e,_) o ts t' -> ProgWrite o (Set.map ((<> Text "\n") . evalPattern (Map.toList e)) ts) t')
    (\(e,_) c l r -> if eval c $ Map.toList e then l else r)
    Terminate
    (Map.fromList ((,[]) <$> vars spec),inputs)
    spec

data RecStruct p a = NoRec | RecSub p a | RecSame p a | RecBoth p a a

sem :: forall st p a.
  (st -> Varname -> ValueSet -> InputMode -> RecStruct p st) -> (RecStruct p a -> a) ->
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
  (st -> Varname -> ValueSet -> InputMode -> m (RecStruct p st)) -> (RecStruct p a -> m a) ->
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
        NoRec -> pure NoRec
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
