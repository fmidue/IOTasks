{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTasks.CodeGeneration.Analysis
  -- (
  -- printProgram,
  -- programIR,
  -- )
  where

import Data.Term
import Data.Term.Typed.AST

import Test.IOTasks.Specification

import Control.Arrow ((***))
import Data.List (isSuffixOf)

import Algebra.Lattice
import Algebra.Lattice.Ordered
import Data.Map (Map)
import qualified Data.Map as Map

type Facts a = Map Varname a

joinFacts :: Lattice a => [Facts a] -> Facts a
joinFacts = joins

type Varname = String

instance Lattice Usage where
  x /\ y = getOrdered $ Ordered x /\ Ordered y
  x \/ y = getOrdered $ Ordered x \/ Ordered y

instance BoundedJoinSemiLattice Usage where
  bottom = getOrdered bottom

instance BoundedJoinSemiLattice Modification where
  bottom = getOrdered bottom

instance Lattice Modification where
  x /\ y = getOrdered $ Ordered x /\ Ordered y
  x \/ y = getOrdered $ Ordered x \/ Ordered y

data Modification = R | W deriving (Show, Eq, Ord, Bounded)

termFacts :: SynTermTyped t (AST (Varname,Usage)) => t a -> Facts Usage
termFacts = go . viewTermTyped where
  go :: AST (Varname,Usage) a -> Facts Usage
  go (Leaf _ _) = Map.empty
  go (Lam _ t) = go (t (undefined, undefined))
  go (Var (x,All) _) = Map.singleton x All
  go (Var (x,Current) _) = Map.singleton x Current
  go (App f x) = joinFacts [go f, go x]

analyse :: (VarListTerm t Varname, SynTermTyped t (AST (Varname,Usage))) => Specification t -> [AnnAction t (Facts (Usage, Modification))]
analyse = annotateSpec $ combineTransfer usageTransfer modTransfer where
  usageTransfer :: SynTermTyped t (AST (Varname,Usage)) => Transfer t Usage
  usageTransfer = Transfer
    { tRead = const id
    , tWrite = \ts is' -> let new = termFacts <$> ts in joinFacts (is':new)
    , tBranch = \c is1 is2 is' -> let new =  termFacts c in joinFacts [new, is1, is2, is']
    , tTillE = \is is' -> joinFacts [is, is']
    , tE = Map.empty
    }
  modTransfer :: VarListTerm t Varname => Transfer t Modification
  modTransfer = Transfer
    { tRead = \v is' -> joinFacts [Map.singleton v W, is']
    , tWrite = \ts is' -> let new = [ (x,R) | x <- concatMap termVars ts ] in joinFacts [Map.fromList new, is']
    , tBranch = \c is1 is2 is' -> let new = [ (x,R) | x <- termVars c ] in joinFacts [Map.fromList new, is1, is2, is']
    , tTillE = \_ is' -> is' --ignore information inside the loop scope but propagate information following the loop
    , tE = Map.empty -- don't let any information from the outside scope in
  }

rootUsageFacts :: [AnnAction t (Facts (Usage, Modification))] -> Facts Usage
rootUsageFacts = Map.map fst . safeHeadFact

normalizeSpec :: Specification t -> Specification t
normalizeSpec (Spec as) = Spec $ go as where
  go (Branch c s1 s2 : as') = [Branch c (s1 <> Spec as') (s2 <> Spec as')]
  go x = x

annotateSpec :: (Ord i, SynTermTyped t (AST (Varname,Usage)))
  => Transfer t i
  -> Specification t
  -> [AnnAction t (Facts i)]
annotateSpec t@Transfer{..} (normalizeSpec -> Spec as) = go as where
  --go :: [Action (Specification t) t] -> [([Fact],Action ([Fact], (Specification t)) t)]
  go [] = [EmptyAction]
  go (ReadInput x ty : s') =
    let r = go s'
    in AnnAction (tRead x (safeHeadFact r)) (ReadInput x ty) : r
  go (WriteOutput o ps ts : s') =
    let r = go s'
    in AnnAction (tWrite ts (safeHeadFact r))  (WriteOutput o ps ts) : r
  go (Branch c s1 s2 : []) =
    let q1 = annotateSpec t s1
        q2 = annotateSpec t s2
    in [AnnAction
        (tBranch c (safeHeadFact q1) (safeHeadFact q2) Map.empty)
        (Branch c q1 q2)]
  go (Branch{} : _) = error "non-normalized spec!"
  go (TillE s : s') =
    let r = go s'
        q = annotateSpec t s
    in AnnAction (tTillE (safeHeadFact q) (safeHeadFact r)) (TillE q) : r
  go (E : _) = [AnnAction tE E]

safeHeadFact :: [AnnAction t (Facts i)] -> Facts i
safeHeadFact [] = Map.empty
safeHeadFact (EmptyAction:_) = Map.empty
safeHeadFact (x:_) = getAnnotation x

data Transfer t i = Transfer
  { tRead :: Varname -> Facts i -> Facts i
  , tWrite :: forall a. [t a] -> Facts i -> Facts i
  , tBranch :: t Bool -> Facts i -> Facts i -> Facts i -> Facts i
  , tTillE :: Facts i -> Facts i -> Facts i
  , tE :: Facts i
  }

combineTransfer :: (BoundedJoinSemiLattice i, BoundedJoinSemiLattice j) => Transfer t i -> Transfer t j -> Transfer t (i,j)
combineTransfer t1 t2 = Transfer
  { tRead = \v -> combine . (tRead t1 v *** tRead t2 v) . partitionFacts
  , tWrite = \ts -> combine . (tWrite t1 ts *** tWrite t2 ts) . partitionFacts
  , tBranch = \c is1 is2 ->
              let (i1,j1) = partitionFacts is1
                  (i2,j2) = partitionFacts is2
              in combine . (tBranch t1 c i1 i2 *** tBranch t2 c j1 j2) . partitionFacts
  , tTillE = \is ->
             let (i,j) = partitionFacts is
             in combine . (tTillE t1 i *** tTillE t2 j) . partitionFacts
  , tE = combine (tE t1,tE t2)
  }
  where
    combine = uncurry (Map.unionWith (\/)) . (fmap (,bottom) *** fmap (bottom,))

partitionFacts :: Facts (a, b) -> (Facts a, Facts b)
partitionFacts = Map.foldrWithKey f (Map.empty,Map.empty) where
  f k (a,b) (as,bs) = (Map.insert k a as , Map.insert k b bs)

data AnnAction t a =  AnnAction
  { getAnnotation :: a
  , _getAction :: Action [AnnAction t a] t
  }
  | EmptyAction
