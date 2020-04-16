{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Test.IOTasks.CodeGeneration.Analysis
  -- (
  -- printProgram,
  -- programIR,
  -- )
  where

import Test.IOTasks.Specification
import Data.Term
import Data.Term.AST
import Test.IOTasks.CodeGeneration.IRGraph

import Control.Arrow ((***), second)
import Control.Monad.State

import Data.Functor.Identity
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.HughesPJClass hiding (first, (<>))

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

termFacts :: SynTerm t (AST Varname) => t a -> Facts Usage
termFacts = go . viewTerm where
  go (Node _ ts) = joinFacts (go <$> ts)
  go (Infix x _ y) = joinFacts [go x, go y]
  go (Lam _ t) = go t
  go (UVar (x,Current)) = Map.singleton x Current
  go (UVar (x,All)) = Map.singleton x All
  go (SVar x) = Map.empty
  go (Literal _) = Map.empty

analyse :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Specification t -> [AnnAction t (Facts (Usage, Modification))]
-- analyse (Spec as) = analyse' as
analyse = annotateSpec $ combineTransfer usageTransfer modTransfer where
  usageTransfer :: SynTerm t (AST Varname) => Transfer t Usage
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

printProgram :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Specification t -> Doc
printProgram = hang (text "p :: IO ()" $$ text "p = do") 2 . printBasicProgram . programIR

programIR :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Specification t -> IRSpine
programIR s = foldr1 (<:>) . fst $ runFreshVarM (mapM (translate (rootUsageFacts x)) x) initState
  where x = analyse s

-- stores next fresh index and a stack of most recent indecies for the current/surounding scopes
-- invariant: stack is never empty
newtype FreshVarM a = FreshVarM { runFreshVarM :: [(Varname, Int)] -> (a,[(Varname, Int)]) }
  deriving (Functor, Applicative, Monad, MonadState [(Varname, Int)]) via (StateT [(Varname, Int)] Identity)

initState :: [(Varname,Int)]
initState = []

updateContext :: Eq k => (k, Int) -> [(k,Int)] -> [(k,Int)]
updateContext (k,v) [] = [(k,v)]
updateContext (k,v) ((k',v') : xs)
  | k == k' = (k',v) : xs
  | otherwise = (k',v') : updateContext (k,v) xs

-- generating a fresh name under the assumption that user defined variables dont end in numberic sequences
-- TODO: not a very good assumption
freshName :: Varname -> FreshVarM IndexedVar
freshName v = do
  i <- gets $ (+1) . fromMaybe 0 . lookup v
  modify $ updateContext (v, i)
  return $ IVar (v, i)

currentName :: Varname -> FreshVarM IndexedVar
currentName v = do
  i <- gets $ fromMaybe 0 . lookup v
  return $ IVar (v, i)

translate :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Facts Usage -> AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRSpine
translate fs (AnnAction _ (ReadInput x _)) =
  case Map.lookup x fs of
    Just Current -> do
      n <- freshName x
      return $ irRead n
    Just All -> do
      v <- freshName "v"
      xk <- currentName x
      xi <- freshName x
      return $ case xk of
        IVar (_,0) -> irRead v <:> irUpdate xi (\_ v' -> Literal $ "["++v'++"]") xk v
        _ -> irRead v <:> irUpdate xi (\xk' v' -> Infix (Literal xk') "++" (Literal $ "["++v'++"]")) xk v
    Nothing -> error "invalid spec"
translate _ (AnnAction _ (WriteOutput True _ _)) = return irNOP
translate fs (AnnAction _ (WriteOutput False _ (t:_))) = do
  ast <- adjustVars fs $ viewTerm t
  return $ irPrint ast
translate _ (AnnAction _ (WriteOutput False _  [])) = error "invalid spec"
translate fs (AnnAction _ (Branch c as1 as2)) = do
  ast <- adjustVars fs $ viewTerm c
  x <- mapM (translate fs) as2
  y <- mapM (translate fs) as1
  return $ irIf ast (foldr1 (<:>) x) (foldr1 (<:>) y)
translate fs (AnnAction _ (TillE as)) = do
  let writeVars = Map.keys . Map.filter ((== W).snd) $ safeHeadFact as
  params <- mapM currentName writeVars -- 1. determine the names to call the loop with
  l <- freshName "loop"
  patternVars <- mapM freshName writeVars -- 2. get the next free names for the loop variables
  body <- translateLoop fs l writeVars as -- 3. translate the loop body
  returnVars <- mapM freshName writeVars -- 4 get names to bind result to
  return $ irDefLoop l patternVars body <:> irEnterLoop l (map SVar params) returnVars
translate _ (AnnAction _ E) = error "E at toplevel"
translate _ EmptyAction = return irNOP

translateLoop :: (VarListTerm t Varname, SynTerm t (AST Varname))  => Facts Usage -> IndexedVar -> [Varname] -> [AnnAction t (Facts (Usage, Modification))] -> FreshVarM IRSpine
translateLoop fs l wVars = (foldr (<:>) irNOP <$>) . mapM go where
  go EmptyAction = do
    params <- mapM currentName wVars
    return $ irRecCall l (map SVar params)
  go (AnnAction _ E) = do
    returnNames <- mapM currentName wVars
    return $ irReturn returnNames
  go (AnnAction _ (Branch c as1 as2)) = do
    ast <- adjustVars fs $ viewTerm c
    x <- translateLoop fs l wVars as2
    y <- translateLoop fs l wVars as1
    return $ irIf ast x y
  go a = translate fs a

adjustVars :: Facts Usage -> AST Varname -> FreshVarM (AST IndexedVar)
adjustVars fs (Node f xs) = do
  ys <- mapM (adjustVars fs) xs
  return $ Node f ys
adjustVars fs (Infix x op y) = do
  x' <- adjustVars fs x
  y' <- adjustVars fs y
  return $ Infix x' op y'
adjustVars fs (UVar (x,u)) = do
    xi <- currentName x
    return $ case u of
      Current ->
        case Map.lookup x fs of
          Just All -> Node "last" [SVar xi]
          Just Current -> SVar xi
          Nothing -> error "invalid spec"
      All -> SVar xi
adjustVars fs (Lam xs t) = do
  t' <- adjustVars fs t
  return $ Lam xs t'
adjustVars _ (SVar x) = do
  xi <- currentName x
  return $ SVar xi
adjustVars _ (Literal x) = return $ Literal x

normalizeSpec :: Specification t -> Specification t
normalizeSpec (Spec as) = Spec $ go as where
  go (Branch c s1 s2 : as') = [Branch c (s1 <> Spec as') (s2 <> Spec as')]
  go x = x

annotateSpec :: (Ord i, SynTerm t (AST Varname))
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
