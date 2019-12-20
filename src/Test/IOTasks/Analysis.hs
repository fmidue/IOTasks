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

analyse :: SynTerm t => Specification t -> [Fact]
analyse (Spec as) = analyse' as

data Level = Top | Loop

analyse' :: SynTerm t => [Action t] -> [Fact]
analyse' [] = []
analyse' (ReadInput _ _: s') = analyse' s'
analyse' (WriteOutput _ _ ts: s') = joinFacts $ analyse' s' : (termFacts <$> ts)
analyse' (Branch c s1 s2 : s') =
  joinFacts
    [ termFacts c
    , analyse s1
    , analyse s2
    , analyse' s']
analyse' (TillE s : s') = joinFacts [analyse s, analyse' s']
analyse' (E : _) = []

printProgram :: (TermVars t, SynTerm t) => Specification t -> Doc
printProgram (Spec as) = hang (text "p = do") 2 (vcat $ map (translate (analyse' as)) as)

translate :: (TermVars t, SynTerm t) => [Fact] -> Action t -> Doc
translate fs (ReadInput x _) =
  case lookup x fs of
    Just C -> text $ x ++ " <- readLn"
    Just A -> text "v <- readLn" $$ text ("modify (store v "++ x ++")")
    Nothing -> error "invalid spec"
translate _ (WriteOutput True _ _) = mempty
translate fs (WriteOutput False _ (t:_)) =
     text ("v <- gets (evalTerm (" ++ applicativeContext fs t ++ "))")
  $$ text "print v"
translate _ (WriteOutput False _  []) = error "invalid spec"
translate fs (Branch c (Spec as1) (Spec as2)) =
  hang (text ("ifM (gets" ++ applicativeContext fs c ++ ")")) 2 $
       hang (text "then do") 2 (vcat (translate fs <$> as1))
    $$ hang (text "else do") 2 (vcat (translate fs <$> as2))
translate fs (TillE (Spec as)) =
  hang (text "let loop = do") 2
    (vcat (translate fs <$> as)
    $$ text "loop")
  $$ text "catchError (\\_ -> return ()) loop"
translate _ E = text "throwError Exit"

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
