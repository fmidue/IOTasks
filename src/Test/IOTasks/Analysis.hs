module Test.IOTasks.Analysis where

import Test.IOTasks.Specification
import Test.IOTasks.Term

import Data.List (sort, groupBy, maximumBy)
import Data.Function (on)

import Text.PrettyPrint.HughesPJ

type Fact = (Varname, Usage)

joinFacts :: [[Fact]] -> [Fact]
joinFacts =
  fmap (maximumBy (compare `on` snd))
  . groupBy ((==) `on` fst)
  . sort
  . concat

type Varname = String

data Usage = C | A deriving (Show, Eq, Ord)

termFacts :: SynTerm t => t a -> [Fact]
termFacts = go . viewTerm where
  go (Node _ ts) = joinFacts (go <$> ts)
  go (Infix x _ y) = joinFacts [go x, go y]
  go (Leaf s) =
    case splitAt (length s - 2) s of
      (x,"_C") -> [(x,C)]
      (x,"_A") -> [(x,A)]
      _        -> []

analyse :: SynTerm t => Specification t -> [AnnAction [Fact] t]
-- analyse (Spec as) = analyse' as
analyse = annotateSpec f where
  f ReadInput{} = []
  f (WriteOutput _ _ ts) = joinFacts $ termFacts <$> ts
  f (Branch c _ _) = termFacts c
  f (TillE _) = []
  f E = []

data Level = Top | Loop

printProgram :: (TermVars t, SynTerm t) => Specification t -> Doc
printProgram s = hang (text "p = do") 2 (vcat $ map (translate (getAnnotation $ head x)) x)
  where x = analyse s

translate :: (TermVars t, SynTerm t) => [Fact] -> AnnAction [Fact] t -> Doc
translate fs (AnnAction _ (ReadInput x _)) =
  case lookup x fs of
    Just C -> text $ x ++ " <- readLn"
    Just A -> text "v <- readLn" $$ text ("modify (store v "++ x ++")")
    Nothing -> error "invalid spec"
translate _ (AnnAction _ (WriteOutput True _ _)) = mempty
translate fs (AnnAction _ (WriteOutput False _ (t:_))) =
     text ("v <- gets (evalTerm (" ++ applicativeContext fs t ++ "))")
  $$ text "print v"
translate _ (AnnAction _ (WriteOutput False _  [])) = error "invalid spec"
translate fs (AnnAction _ (Branch c as1 as2)) =
  hang (text ("ifM (gets" ++ applicativeContext fs c ++ ")")) 2 $
       hang (text "then do") 2 (vcat (translate fs <$> as1))
    $$ hang (text "else do") 2 (vcat (translate fs <$> as2))
translate fs (AnnAction _ (TillE as)) =
  hang (text "let loop = do") 2
    (vcat (translate fs <$> as)
    $$ text "loop")
  $$ text "catchError (\\_ -> return ()) loop"
translate _ (AnnAction _ E) = text "throwError Exit"

flattenAST :: AST -> String
flattenAST = go "" "" where
  go l r (Node s ts) = l ++ s ++ " " ++ unwords (map (go "(" ")") ts) ++ r
  go l r (Infix x op y) = l ++ go "(" ")" x ++ " " ++ op ++  " " ++  go "(" ")" y ++ r
  go _ _ (Leaf s) = s

adjustVars :: [Fact] -> AST -> AST
adjustVars fs (Node f xs) = Node f $ adjustVars fs <$> xs
adjustVars fs (Infix x op y) = Infix (adjustVars fs x) op (adjustVars fs y)
adjustVars fs (Leaf s) =
    case splitAt (length s - 2) s of
      (x,"_C") ->
        case lookup x fs of
          Just A -> Node "last" [Leaf x]
          Just C -> Leaf x
          Nothing -> error "invalid spec"
      (x,"_A") -> Leaf x
      _ -> Leaf s

applicativeContext :: (SynTerm t, TermVars t) => [Fact] -> t a -> String
applicativeContext fs t =
  let vs = map ("getAll " ++) $ filter (\v -> lookup v fs == Just A) (termVars t)
  in "(\\" ++ unwords vs ++ " -> " ++ flattenAST (adjustVars fs $ viewTerm t) ++ ") <$> " ++ head vs ++ concatMap ( ++ " <*> ") (tail vs)

annotateSpec :: SynTerm t
  => (Action (Specification t) t -> [Fact])
  -> Specification t
  -> [AnnAction [Fact] t]
annotateSpec f (Spec as) = go as where
  --go :: [Action (Specification t) t] -> [([Fact],Action ([Fact], (Specification t)) t)]
  go [] = []
  go (a@(ReadInput x ty): s') =
    let r = go s'
    in AnnAction (safeJoinWithHeads (f a) [r]) (ReadInput x ty) : r
  go (a@(WriteOutput o ps ts): s') =
    let r = go s'
    in AnnAction (safeJoinWithHeads (f a) [r])  (WriteOutput o ps ts) : r
  go (a@(Branch c s1 s2) : s') =
    let r = go s'
        q1 = annotateSpec f s1
        q2 = annotateSpec f s2
    in AnnAction
        (safeJoinWithHeads (f a) [ q1, q2, r])
        (Branch c q1 q2) : r
  go (a@(TillE s) : s') =
    let r = go s'
        q = annotateSpec f s
    in AnnAction (safeJoinWithHeads (f a) [q, r]) (TillE q) : r
  go (E : s) = AnnAction (f E) E : go s

safeJoinWithHeads :: [Fact] -> [[AnnAction [Fact] t]] -> [Fact]
safeJoinWithHeads f = joinFacts .  (f :) . map safeHeadFact where
  safeHeadFact [] = []
  safeHeadFact (x:_) = getAnnotation x

data AnnAction a t =  AnnAction
  { getAnnotation :: a
  , getAction :: Action [AnnAction a t] t
  }
