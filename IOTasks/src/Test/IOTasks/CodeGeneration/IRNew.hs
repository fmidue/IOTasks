{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.IRNew where

import Data.Maybe (fromJust, fromMaybe)
import Data.List (sort, group, intercalate)
import Data.Proxy
import Type.Reflection (Typeable, eqTypeRep, typeRep, (:~~:)(..))

import Data.Environment
import Data.Term
import Data.Term.Liftable
import qualified Data.Term.Liftable.Prelude as T
import Data.Term.Typed.AST

import Text.PrettyPrint.HughesPJClass (Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP

data Instruction
  = READ Var
  | PRINT Var
  | IF Var [Instruction] [Instruction]
  | TAILCALL Var [Var] -- TAILCALL f [x1,..,xn] => f x1 .. xn
  | BINDCALL Var [Var] [Var] -- BINDCALL f [x1,..,xn] [v1,..,vm] => (v1,..,vm) <- f x1 .. xn
  | YIELD [Var]
  | NOP
  deriving (Show,Eq)

type Var = String

type AM = ([Instruction],([Def ()],[F]),Vals,[[Var]],[Input],[Output])

type IRProgram = ([Instruction], [Def ()], [F])
type Vals = Environment Var
type Def a = (Var,DefRhs a,Int)
data DefRhs :: * -> * where
  Forget :: Typeable a => DefRhs a -> DefRhs ()
  U :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> Var -> Var -> DefRhs c
  N1L :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> DefRhs a -> Var -> DefRhs c
  N1R :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> Var -> DefRhs b -> DefRhs c
  N2  :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> DefRhs a -> DefRhs b -> DefRhs c
  Const :: AST Var a -> DefRhs a
type F = (Var, [Var], [Instruction])
type Input = Int
data Output = I Int | O Int deriving (Show,Eq) -- simple trace elements

-- very basic sequencing, assumes well-formed input, i.e. not duplicate varaiable definitions etc.
(<:>) :: IRProgram -> IRProgram -> IRProgram
(is1,ds1,fs1) <:> (is2,ds2,fs2) = (is1 ++ is2,foldr (\(x,d,_) -> addDef (x,d)) ds1 ds2,fs1 ++ fs2)

readIR :: Var -> IRProgram
readIR x = ([READ x],[],[])

initialValueIR :: Typeable a => Var -> Var -> Var -> Proxy a -> IRProgram
initialValueIR x y v (Proxy :: Proxy a) = ([],[(x,Forget $ U @[a] @[a] @a (const (`T.cons` T.nil)) y v,0)],[])

updateIR :: Typeable a => Var -> Var -> Var -> Proxy a -> IRProgram
updateIR x y v (Proxy :: Proxy a) = ([],[(x, Forget $ U @[a] (T.++) y v,0)],[])

printIR :: Var -> IRProgram
printIR x = ([PRINT x],[],[])

valueDefIR :: Typeable a => Var -> AST Varname a -> IRProgram
valueDefIR x t = ([],[(x,Forget $ Const t,0)],[])

ifIR :: Var -> IRProgram -> IRProgram -> IRProgram
ifIR c (is1,ds1,fs1) (is2,ds2,fs2) = ([IF c is1 is2],ds1 ++ ds2,fs1 ++ fs2)

defLoopIR :: Var -> [Var] -> IRProgram -> IRProgram
defLoopIR f xs (b,ds,fs) = ([],ds,(f,xs,b):fs)

enterLoopIR :: Var -> [Var] -> [Var] -> IRProgram
enterLoopIR f ps rvs = ([BINDCALL f ps rvs],[],[])

recCallIR :: Var -> [Var] -> IRProgram
recCallIR f ps = ([TAILCALL f ps],[],[])

yieldIR :: [Var] -> IRProgram
yieldIR rvs = ([YIELD rvs],[],[])

nopIR :: IRProgram
nopIR = ([NOP],[],[])

runInstructions :: [Instruction] -> ([Def ()],[F]) -> [Input] -> [Output]
runInstructions p d i = let (_,_,_,_,_,o) = runAM (p,d,emptyEnvironment,[],i,[]) in o

runAM :: AM -> AM
runAM ([],d,vs,rvs,i,o) = ([],d,vs,rvs,i,o)
runAM m = runAM $ stepAM m

stepAM :: AM -> AM
stepAM ([],_,_,_,_,_) = error "no instruction to execute"
stepAM (READ _:_,_,_,_,[],_) = error "not enough inputs"
stepAM (READ x:p',d,vs,rvs,i:is,o) = (p',d,updateValue (x,i) vs,rvs,is,I i:o)
stepAM (PRINT x:p',d,vs,rvs,i,o) = (p',d,vs,rvs,i,O (_intValue x d vs):o)
stepAM (IF x t e:p',d,vs,rvs,i,o)
  | _boolValue x d vs = (t++p',d,vs,rvs,i,o)
  | otherwise = (e++p',d,vs,rvs,i,o)
stepAM (TAILCALL l ps:p',d,vs,rvs,i,o) = (_getInstructions l d ++ p',d,_setParameters l ps d vs,rvs,i,o)
stepAM (BINDCALL l ps bs:p',d,vs,rvs,i,o) = (_getInstructions l d ++ p',d,_setParameters l ps d vs,bs:rvs,i,o)
stepAM (YIELD _:_,_,_,[],_,_) = error "missing binders for returned values"
stepAM (YIELD xs:p',d,vs,rvs:rvss,i,o)
  | _length vs == length rvs = (p', d,updateValues (zipWith (\x rv -> (rv,_getPayload x vs)) xs rvs) vs,rvss,i,o)
  | otherwise = error "wrong number of binders for returned values"
stepAM (NOP:p',d,vs,rvs,i,o) = (p',d,vs,rvs,i,o)

updateValues :: [(Var, a)] -> Vals -> Vals
updateValues xs ys = fromJust $ foldr _ (Just ys) xs

updateValue :: (Typeable a, Show a) => (Var,a) -> Vals -> Vals
updateValue (x,v) = fromJust . store x show v

-- printing programs
printIRProgram :: IRProgram -> Doc
printIRProgram (is,ds,fs) =
        PP.text "-- Main --"
  PP.$$ PP.vcat (map printInstruction is)
  PP.$$ PP.text "-- Defs --"
  PP.$$ PP.vcat (map printDef ds)
  PP.$$ PP.text "-- Loops --"
  PP.$$ PP.vcat (map printF fs)

printInstruction :: Instruction -> Doc
printInstruction (READ x) = PP.text $ "READ " ++ x
printInstruction (PRINT t) = PP.text $ "PRINT " ++ t
printInstruction (IF c t e) =
  PP.hang (PP.text ("IF " ++ c)) 2
    ( PP.hang (PP.text "THEN") 2 (PP.vcat (map printInstruction t))
    PP.$$ PP.hang (PP.text "ELSE") 2 (PP.vcat (map printInstruction e))
    )
printInstruction (TAILCALL f ps) = PP.text ("TAILCALL " ++ f) PP.<+> tupelize ps
printInstruction (BINDCALL f ps rvs) = PP.text ("BINDCALL " ++ f) PP.<+> tupelize ps PP.<+> tupelize rvs
printInstruction (YIELD rvs) = PP.text "RETURN " PP.<+> tupelize rvs
printInstruction NOP = mempty

printDef :: Def a -> Doc
printDef (x,rhs,_) = PP.text (x ++ " :=") PP.<+> go rhs where
  go :: DefRhs a -> Doc
  go (Forget d) = go d
  go (U f y v) = PP.text $ printFlat $ f (Leaf undefined y) (Leaf undefined v)
  go (N1L f y v) = PP.text $ printFlat $ f (toAST y) (Leaf undefined v)
  go (N1R f y v) = PP.text $ printFlat $ f (Leaf undefined y) (toAST v)
  go (N2 f y v) = PP.text $ printFlat $ f (toAST y) (toAST v)
  go (Const t) = PP.text $ printFlat t

printF :: F -> Doc
printF (f,xs,b) = PP.hang (PP.text f PP.<+> tupelize xs PP.<+> PP.text ":=") 2 $
  PP.vcat (map printInstruction b)

tupelize :: [String] -> Doc
tupelize [] = PP.text ""
tupelize [v] = PP.text v
tupelize vs = PP.text $ "(" ++ intercalate "," vs ++ ")"

-- optimizing IRProgram
optimize :: IRProgram -> IRProgram
optimize (i,d,f) = (i,d',f)
  where d' = simplify (emptyEnvironment @Environment) d

-- manipulation of DefRhs
printDefTree :: DefRhs a -> String
printDefTree = pPrintTree . toAST

toAST :: DefRhs a -> AST Var a
toAST (Forget x) = App (unHO $ \_ -> Leaf () "()") $ toAST x
toAST (U f y v) = f (variable y) (variable v)
toAST (N1L f y v) = f (toAST y) (variable v)
toAST (N1R f y v) = f (variable y) (toAST v)
toAST (N2 f y v) = f (toAST y) (toAST v)
toAST (Const t) = t

instance Show (DefRhs a) where
  show (Forget x) = "Forget " ++ show x
  show (U f y v) = "U ? " ++ y ++ " " ++ v
  show (N1L f y v) = "N1L ? " ++ v
  show (N1R f y v) = "N1R ? " ++ y
  show (N2 f y v) = "N2 ?"
  show (Const t) = "Cosnt ?"

simplify :: (VarEnv env Var, Typeable a) => env Var -> [Def a] -> [Def a]
simplify e ds = case inline1 ds of
  Just ds' -> simplify e ds'
  Nothing -> map (\(x,d,n) -> (x,reduceD e d,n)) ds

reduceD :: VarEnv env Var => env Var -> DefRhs a -> DefRhs a
reduceD e (Forget x) = Forget (reduceD e x)
reduceD e (U f y v) = U (\y v  -> reduceT e $ f y v) y v
reduceD e (N1L f y v) = N1L (\y v  -> reduceT e $ f y v) y v
reduceD e (N1R f y v) = N1R (\y v  -> reduceT e $ f y v) y v
reduceD e (N2 f y v) = N2 (\y v  -> reduceT e $ f y v) y v
reduceD e (Const t) = Const (reduceT e t)

inline :: (Typeable x, Typeable a) => (Var,DefRhs x) -> DefRhs a -> Maybe (DefRhs a)
inline (x,t :: DefRhs x) (Forget y) = Forget <$> inline (x,t) y
inline (x,t :: DefRhs x) (U (f :: AST Var a -> AST Var b -> AST Var c) y v)
  | x == y = case eqTypeRep (typeRep @x) (typeRep @a) of
    Just HRefl -> Just $ N1L f t v
    Nothing -> Nothing
  | x == v = case eqTypeRep (typeRep @x) (typeRep @b) of
    Just HRefl -> Just $ N1R f y t
    Nothing -> Nothing
  | otherwise = Nothing
inline (x,t :: DefRhs x) (N1L (f :: AST Var a -> AST Var b -> AST Var c) y v)
  | x == v = case eqTypeRep (typeRep @x) (typeRep @b) of
    Just HRefl -> Just $ N2 f y t
    Nothing -> Nothing
  | otherwise = (\y' -> N1L f y' v) <$> inline (x,t) y
inline (x,t :: DefRhs x) (N1R (f :: AST Var a -> AST Var b -> AST Var c) y v)
  | x == y = case eqTypeRep (typeRep @x) (typeRep @a) of
    Just HRefl -> Just $ N2 f t v
    Nothing -> Nothing
  | otherwise = N1R f y <$> inline (x,t) v
inline (x,t) (N2 f y v)
  = case (inline (x,t) y, inline (x,t) v) of
    (Nothing, Nothing) -> Nothing
    (Just y', Nothing) -> Just $ N2 f y' v
    (Nothing, Just v') -> Just $ N2 f y v'
    -- this probably does not happen, at least not
    -- if only variables with 1 occurence are inlined
    (Just y', Just v') -> Just $ N2 f y' v'
inline _ (Const t) = Nothing

inline1 :: Typeable a => [Def a] -> Maybe [Def a]
inline1 ds =
  case break (\(_,_,n) -> n == 1) ds of
    (_,[]) -> Nothing
    (xs,(x,Forget t,_):ys) -> Just $ foldr (f (x,t)) [] (xs++ys)
    (xs,(x,t,_):ys) -> Just $ foldr (f (x,t)) [] (xs++ys)
  where
    f x (y,d,n) ds' = case inline x d of
      Nothing -> (y,d,n) : ds'
      Just d' -> (y,d',n) : ds'

addDef :: (Var,DefRhs a) -> [Def a] -> [Def a]
addDef (x,d) ds =
  let n = fromMaybe 0 $ lookup x $ usedVars (map (\(_,d,_) -> d) ds)
  in (x,d,n) : updateUseCount (usedVars [d]) ds

updateUseCount :: [(Var,Int)] -> [Def a] -> [Def a]
updateUseCount cs = foldr phi [] where
  phi (x,d,n) ds = case lookup x cs of
    Just m -> (x,d,n+m) : ds
    Nothing -> (x,d,n) : ds

definedVars :: [Def a] -> [Var]
definedVars = map (\(x,_,_) -> x)

usedVars :: [DefRhs a] -> [(Var,Int)]
usedVars = map (\x -> (head x, length x)) . group . sort . concatMap go where
  go :: DefRhs a -> [Var]
  go (Forget d) = go d
  go (U _ y v) = [y,v]
  go (N1L _ y v) = v : go y
  go (N1R _ y v) = y : go v
  go (N2 _ y v) = go y ++ go v
  go (Const t) = termVars t

-- vorher
ds  :: [Def ()]
ds =
  [ ("x", Forget $ U @Int (T.+) "v1" "y", 1)
  , ("z", Forget $ U @Int (T.+) "a" "v3", 0)
  , ("b", Forget $ U @Bool @Int (T.>) "x" "y1",0)
  ]

d = ("a", Forget $ U @Int (T.+) "z" "v2")
-- nachher
ds'  :: [Def ()]
ds' = [ ("x", Forget $ N1L @Int (T.+) (N1L (T.+) (U (T.+) "a" "v3") "a") "v1" , 0) ]
