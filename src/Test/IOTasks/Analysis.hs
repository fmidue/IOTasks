{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Analysis where

import Test.IOTasks.Specification
import Test.IOTasks.Term
import Test.IOTasks.Utils
import Test.IOTasks.IR

import Control.Arrow ((***), first, second)
import Control.Monad.State

import Data.Functor.Identity
import Data.List (sort, groupBy, maximumBy, intercalate)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.HughesPJClass hiding (first, (<>))

type Fact a = (Varname, a)

-- does not work correctly for Either a b
joinFacts :: Ord a => [[Fact a]] -> [Fact a]
joinFacts =
  fmap (maximumBy (compare `on` snd))
  . groupBy ((==) `on` fst)
  . sort
  . concat

type Varname = String

data Usage = C | A deriving (Show, Eq, Ord)
data Modification = R | W deriving (Show, Eq, Ord)

termFacts :: SynTerm t => t a -> [Fact Usage]
termFacts = go . viewTerm where
  go (Node _ ts) = joinFacts (go <$> ts)
  go (Infix x _ y) = joinFacts [go x, go y]
  go (Leaf s) =
    case splitAt (length s - 2) s of
      (x,"_C") -> [(x,C)]
      (x,"_A") -> [(x,A)]
      _        -> []

analyse :: (TermVars t, SynTerm t) => Specification t -> [AnnAction t [Fact (Either Usage Modification)]]
-- analyse (Spec as) = analyse' as
analyse = annotateSpec $ combineTransfer usageTransfer modTransfer where
  usageTransfer :: SynTerm t => Transfer t Usage
  usageTransfer = Transfer
    { tRead = const id
    , tWrite = \ts is' -> let new = termFacts <$> ts in joinFacts (is':new)
    , tBranch = \c is1 is2 is' -> let new =  termFacts c in joinFacts [new, is1, is2, is']
    , tTillE = \is is' -> joinFacts [is, is']
    , tE = []
    }
  modTransfer :: TermVars t => Transfer t Modification
  modTransfer = Transfer
    { tRead = \v is' -> joinFacts [[(v,W)],is']
    , tWrite = \ts is' -> let new = [ (x,R) | x <- concatMap termVars ts ] in joinFacts [new, is']
    , tBranch = \c is1 is2 is' -> let new = [ (x,R) | x <- termVars c ] in joinFacts [new, is1, is2, is']
    , tTillE = \_ is' -> is' --ignore information inside the loop scope but propagate information following the loop
    , tE = [] -- don't let any information from the outside scope in
  }

rootUsageFacts :: [AnnAction t [Fact (Either Usage Modification)]] -> [Fact Usage]
rootUsageFacts = concatMap (\case (v,Left u) -> [(v,u)]; _ -> []) . safeHeadFact

printProgram :: (TermVars t, SynTerm t) => Specification t -> Doc
printProgram s = hang (text "p :: IO ()" $$ text "p = do") 2 (vcat . (map pPrint) . fst $ runFreshVarM (mapM (translate (rootUsageFacts x)) x) (initState (specVars s)))
  where x = analyse s

-- stores next fresh index and a stack of most recent indecies for the current/surounding scopes
-- invariant: stack is never empty
newtype FreshVarM a = FreshVarM { runFreshVarM :: [(Varname, IndexContext)] -> (a,[(Varname, IndexContext)]) }
  deriving (Functor, Applicative, Monad, MonadState [(Varname, IndexContext)]) via (StateT [(Varname, IndexContext)] Identity)

type IndexContext = (Int, [Int])

initState :: [Varname] -> [(Varname,IndexContext)]
initState = map (,defaultContext)

defaultContext :: IndexContext
defaultContext = (1,[-1])

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

update :: Eq k => k -> (v -> v) -> [(k,v)] -> [(k,v)]
update _ _ [] = []
update k f ((k',v) : xs)
  | k == k' = (k',f v) : update k f xs
  | otherwise = (k',v) : update k f xs

-- generating a fresh name under the assumption that user defined variables dont end in numberic sequences
freshName :: Varname -> FreshVarM Varname
freshName v = do
  i <- gets $ fst . fromMaybe defaultContext . lookup v
  modify $ update v (\(next,_:xs) -> (next+1, next:xs))
  return $ v ++ show i

currentName :: Varname -> FreshVarM Varname
currentName v = do
  i <- gets $ head . snd . fromMaybe defaultContext . lookup v
  case i of
    -1 -> return "[]" -- TODO: get rid of this encoding with better types!
    0   -> return v
    _   -> return $ v ++ show i

enterScope :: FreshVarM ()
enterScope = modify $ map (second $ second (0:))

leaveScope :: FreshVarM ()
leaveScope = modify $ map (second $ second tail)

translate :: (TermVars t, SynTerm t) => [Fact Usage] -> AnnAction t [Fact (Either Usage Modification)] -> FreshVarM (IR l)
translate fs (AnnAction _ (ReadInput x _)) =
  case lookup x fs of
    Just C -> return $ READ x
    Just A -> do
      xk <- currentName x
      xi <- freshName x
      return $ READ "v" `SEQ` UPDATE xi xk
    Nothing -> error "invalid spec"
translate _ (AnnAction _ (WriteOutput True _ _)) = return NOP
translate fs (AnnAction _ (WriteOutput False _ (t:_))) = do
  ast <- adjustVars fs $ viewTerm t
  return $ PRINT ast
translate _ (AnnAction _ (WriteOutput False _  [])) = error "invalid spec"
translate fs (AnnAction _ (Branch c as1 as2)) = do
  enterScope
  x <- mapM (translate fs) as2
  leaveScope
  enterScope
  y <- mapM (translate fs) as1
  leaveScope
  ast <- adjustVars fs $ viewTerm c
  return $ IF ast (foldr SEQ NOP x) (foldr SEQ NOP y)
translate fs (AnnAction _ (TillE as)) = do
  let writeVars = concatMap (\case (x,Right W) -> [x]; _ -> []) (safeHeadFact as)
  enterScope
  body <- translateLoop fs writeVars as
  leaveScope
  params <- mapM currentName writeVars
  returnVars <- mapM freshName writeVars
  return $ DEFLOOP writeVars params returnVars body
  -- $$ text "catchError (\\_ -> return ()) loop"
translate _ (AnnAction _ E) = error "E at toplevel"
translate _  EmptyAction = return NOP

translateLoop :: (TermVars t, SynTerm t)  => [Fact Usage] ->  [Varname] -> [AnnAction t [Fact (Either Usage Modification)]] -> FreshVarM (IR 'Body)
translateLoop fs wVars = (foldr SEQ NOP <$>) . mapM go where
  go EmptyAction = do
    params <- mapM currentName wVars
    return $ CALLLOOP params
  go (AnnAction _ E) =
    return $ RETURN wVars
  go (AnnAction _ (Branch c as1 as2)) = do
    enterScope
    x <- translateLoop fs wVars as2
    leaveScope
    enterScope
    y <- translateLoop fs wVars as1
    leaveScope
    ast <- adjustVars fs $ viewTerm c
    return $ IF ast x y
  go a = translate fs a

mkReturnTuple :: [Varname] -> Doc
mkReturnTuple vs = maybeParens (length vs /= 1) (text (intercalate "," vs))

adjustVars :: [Fact Usage] -> AST -> FreshVarM AST
adjustVars fs (Node f xs) = do
  ys <- mapM (adjustVars fs) xs
  return $ Node f ys
adjustVars fs (Infix x op y) = do
  x' <- adjustVars fs x
  y' <- adjustVars fs y
  return $ Infix x' op y'
adjustVars fs (Leaf s) = do
    let (x,suff) = splitAt (length s - 2) s
    xi <- currentName x
    return $ case suff of
      "_C" ->
        case lookup x fs of
          Just A -> Node "last" [Leaf xi]
          Just C -> Leaf xi
          Nothing -> error "invalid spec"
      "_A" -> Leaf xi
      _ -> Leaf s

-- obsolete
-- applicativeContext :: (SynTerm t, TermVars t) => [Fact Usage] -> t a -> String
-- applicativeContext fs t =
--   let vs = map ("getAll " ++) $ filter (\v -> lookup v fs == Just A) (termVars t)
--   in "(\\" ++ unwords vs ++ " -> " ++ flattenAST (adjustVars fs $ viewTerm t) ++ ") <$> " ++ head vs ++ concatMap ( ++ " <*> ") (tail vs)

normalizeSpec :: Specification t -> Specification t
normalizeSpec (Spec as) = Spec $ go as where
  go (Branch c s1 s2 : as') = [Branch c (s1 <> Spec as') (s2 <> Spec as')]
  go x = x


annotateSpec :: (Ord i, SynTerm t)
  => Transfer t i
  -> Specification t
  -> [AnnAction t [Fact i]]
annotateSpec t@Transfer{..} (Spec as) = go as where
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
        (tBranch c (safeHeadFact q1) (safeHeadFact q2) [])
        (Branch c q1 q2)]
  go (Branch{} : _) = error "non-normalized spec!"
  go (TillE s : s') =
    let r = go s'
        q = annotateSpec t s
    in AnnAction (tTillE (safeHeadFact q) (safeHeadFact r)) (TillE q) : r
  go (E : _) = [AnnAction tE E]

safeHeadFact :: [AnnAction t [Fact i]] -> [Fact i]
safeHeadFact [] = []
safeHeadFact (EmptyAction:_) = []
safeHeadFact (x:_) = getAnnotation x

safeJoinWithHeads :: Ord i => [Fact i] -> [[AnnAction t [Fact i]]] -> [Fact i]
safeJoinWithHeads f = joinFacts .  (f :) . map safeHeadFact

data Transfer t i = Transfer
  { tRead :: Varname -> [Fact i] -> [Fact i]
  , tWrite :: forall a. [t a] -> [Fact i] -> [Fact i]
  , tBranch :: t Bool -> [Fact i] -> [Fact i] -> [Fact i] -> [Fact i]
  , tTillE :: [Fact i] -> [Fact i] -> [Fact i]
  , tE :: [Fact i]
  }

combineTransfer :: Transfer t i -> Transfer t j -> Transfer t (Either i j)
combineTransfer t1 t2 = Transfer
  { tRead = \v -> uncurry (++) . (map (fmap Left) *** map (fmap Right)) . (tRead t1 v *** tRead t2 v) . partitionEitherFacts
  , tWrite = \ts -> uncurry (++) . (map (fmap Left) *** map (fmap Right)) . (tWrite t1 ts *** tWrite t2 ts) . partitionEitherFacts
  , tBranch = \c is1 is2 ->
              let (i1,j1) = partitionEitherFacts is1
                  (i2,j2) = partitionEitherFacts is2
              in uncurry (++) . (map (fmap Left) *** map (fmap Right)) . (tBranch t1 c i1 i2 *** tBranch t2 c j1 j2) . partitionEitherFacts
  , tTillE = \is ->
             let (i,j) = partitionEitherFacts is
             in uncurry (++) . (map (fmap Left) *** map (fmap Right)) . (tTillE t1 i *** tTillE t2 j) . partitionEitherFacts
  , tE = uncurry (++) . (map (fmap Left) *** map (fmap Right)) $ (tE t1,tE t2)
  }

partitionEitherFacts :: [Fact (Either a b)] -> ([Fact a], [Fact b])
partitionEitherFacts = foldr f ([],[]) where
  f :: Fact (Either a b) -> ([Fact a],[Fact b]) -> ([Fact a],[Fact b])
  f (v, Left x) (as,bs) = ((v,x):as,bs)
  f (v, Right x) (as,bs) = (as,(v,x):bs)

data AnnAction t a =  AnnAction
  { getAnnotation :: a
  , getAction :: Action [AnnAction t a] t
  }
  | EmptyAction
