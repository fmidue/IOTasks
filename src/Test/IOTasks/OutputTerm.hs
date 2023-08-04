{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTasks.OutputTerm (
  OutputTerm ,
  SomeOutputTerm(..), withSomeOutputTerm,
  outputTermVarExps, transparentSubterms,
  Test.IOTasks.OutputTerm.oEval,
  ) where

import Prelude hiding (all)

import Test.IOTasks.Terms
import Test.IOTasks.Internal.ConditionTerm
import Test.IOTasks.Internal.Overflow
import Test.IOTasks.ValueMap

import Data.Express (Expr((:$)), var, val, value, (//-), evl, vars, isVar, showExpr)
import Data.List (nub, intercalate)
import Data.Function (on)
import Data.Maybe (maybeToList)
import Data.Bifunctor (first)

import Type.Reflection
import Data.Kind (Type)

import Text.Parsec (parse, char, many1, alphaNum, sepBy1, (<|>), string, digit)
import Text.Parsec.String (Parser)
import Data.Char (isAlphaNum)

data OutputTerm a
  = Transparent (ConditionTerm a)
  | Opaque Expr [[SomeVar]] [SomeTerm]

toExpr :: Typeable a =>  OutputTerm a -> Expr
toExpr (Transparent t) = termExpr t
toExpr (Opaque expr _ _) = expr

outputTermVarExps :: Typeable a => OutputTerm a -> [[SomeVar]]
outputTermVarExps (Transparent t) = termVarExps t
outputTermVarExps (Opaque _ vars _) = vars

transparentSubterms :: Typeable a => OutputTerm a -> [SomeTerm]
transparentSubterms (Transparent t) = subTerms t
transparentSubterms (Opaque _ _ ts) = ts

-- simple instance lifting based on Expr's instances
instance Typeable a => Show (OutputTerm a) where
  show = show . toExpr

instance Typeable a => Eq (OutputTerm a) where
  (==) = (==) `on` toExpr

instance Typeable a => Ord (OutputTerm a) where
  compare = compare `on` toExpr

instance Accessor OutputTerm where
  valueBefore n x = checkNames x $ Transparent $ Current x n
  valuesBefore n x = checkNames x $ Transparent $ All x n

data SomeOutputTerm where
  SomeOutputTerm :: Typeable a => OutputTerm a -> SomeOutputTerm

withSomeOutputTerm :: SomeOutputTerm -> (forall a. Typeable a => OutputTerm a -> r) -> r
withSomeOutputTerm (SomeOutputTerm t) f = f t

checkNames :: VarExp e => e -> a -> a
checkNames = foldr (f . someVarname) id . toVarList
  where
    f x c = if legalVar x then c else error $ "illegal variable name: " ++ x ++ "\variable names can only contain letters, digits, _ and '"

legalVar :: String -> Bool
legalVar = any (\c -> isAlphaNum c || c == '_' || c == '\'')

currentE :: VarExp e => e -> Int -> Expr
currentE x n = case varExpType x of
  Just (SomeTypeRep (ty :: TypeRep (a :: k))) -> withTypeable ty $ withTypeable (typeRepKind ty) $
    case eqTypeRep (typeRep @k) (typeRep @Type) of
      Just HRefl -> Data.Express.var ("[" ++ intercalate "," (map someVarname $ toVarList x) ++ "]_C^" ++ show n) (undefined :: a)
      Nothing -> error $ "currentE: a does not have kind Type in TypeRep a, with a = " ++ show (typeRep @a)
  Nothing -> error "currentE: inconsistent VarExp type"

allE :: VarExp e => e -> Int -> Expr
allE x n = case varExpType x of
  Just (SomeTypeRep (ty :: TypeRep (a :: k))) -> withTypeable ty $ withTypeable (typeRepKind ty) $
    case eqTypeRep (typeRep @k) (typeRep @Type) of
      Just HRefl -> Data.Express.var ("[" ++ intercalate "," (map someVarname $ toVarList x) ++ "]_A^" ++ show n) (undefined :: [a])
      Nothing -> error $ "allE: a does not have kind Type in TypeRep a, with a = " ++ show (typeRep @a)
  Nothing -> error "allE: inconsistent VarExp type"

oEval :: forall a. Typeable a => ValueMap -> OutputTerm a -> (OverflowWarning, a)
oEval = evalOverflow (OverflowTreatment evalSingle evalList) where
  evalSingle :: ValueMap -> OutputTerm Integer -> Either (SubCheck OutputTerm I) I
  evalSingle e (Transparent t) = first (modifySubCheck Transparent) $ evalI e t
  evalSingle e (Opaque expr vss _) = Right $ fromInteger $ eval' expr vss e
  evalList :: ValueMap -> OutputTerm [Integer] -> Either (SubCheck OutputTerm [I]) [I]
  evalList e (Transparent t) = Right $ evalIs e t
  evalList e (Opaque expr vss _) = Right $ fromInteger <$> eval' expr vss e

eval' :: Typeable a => Expr -> [[SomeVar]] -> ValueMap -> a
eval' expr xss e = evl . fillAVars xss e . reduceAVarsIndex e . replaceCVars e $ expr

instance EffectEval OutputTerm where
  type Env OutputTerm = ValueMap
  pureEval f e (Transparent t) = pureEval (f . Transparent) e t
  pureEval _ e (Opaque expr vss _) = pure $ eval' expr vss e

-- evaluation preprocessing

-- replace <var>_C^n with head(<var>_A^n)
replaceCVars :: ValueMap -> Expr -> Expr
replaceCVars m expr = expr //-
  [ (oldExpr, headF ty :$ allE (varnameVarList x m) n)
  | Just (oldExpr, (n,x)) <- map (varStruct C) (vars expr)
  , ty <- maybeToList $ varnameTypeRep x m
  ]

-- given SomeTypeRep of a build Expr for head :: [a] -> a
headF :: SomeTypeRep -> Expr
headF (SomeTypeRep (ta :: TypeRep (a :: k))) =
    withTypeable ta $
      withTypeable (typeRepKind ta) $
        case eqTypeRep (typeRep @k) (typeRep @Type) of
          Just HRefl -> value "head" (head :: [a] -> a)
          Nothing -> error $ "a does not have kind Type in TypeRep a, with a = " ++ show (typeRep @a)

-- replace <var>_A^n with reverse(tail^n(<var>_A^0))
reduceAVarsIndex :: ValueMap -> Expr -> Expr
reduceAVarsIndex m expr = expr //-
  [ (expr, tailF ty n :$ allE (varnameVarList x m) 0)
  | Just (expr, (n,x)) <- map (varStruct A) (vars expr)
  , ty <- maybeToList $ varnameTypeRep x m
  ]

-- given SomeTypeRep of a build Expr for reverse . replicateF tail n :: [a] -> [a]
tailF :: SomeTypeRep -> Int -> Expr
tailF (SomeTypeRep (ta :: TypeRep (a :: k))) n =
  withTypeable ta $
    withTypeable (typeRepKind ta) $
      case eqTypeRep (typeRep @k) (typeRep @Type) of
        Just HRefl -> value ("tail^"++show n) (reverse . replicateF tail n :: [a] -> [a])
        Nothing -> error $ "a does not have kind Type in TypeRep a, with a = " ++ show (typeRep @a)

replicateF :: (a -> a) -> Int -> a -> a
replicateF f n = foldr (.) id $ replicate n f

data AccessType = C | A deriving Show

varStruct :: AccessType -> Expr -> Maybe (Expr,(Int,[Varname]))
varStruct acc x
  | isVar x = either (const Nothing) (Just . (x,)) $ parse (varParser acc) "" (reverse $ showExpr x)
  | otherwise = Nothing
  where
    varParser :: AccessType -> Parser (Int,[Varname]) -- parses a variable's string representation in reverse
    varParser acc = do
      n <- many1 digit
      _ <- string ("^"++show acc++"_")
      _ <- char ']'
      x <- sepBy1 (many1 (alphaNum <|> char '_' <|> char '\'')) (char ',')
      _ <- char '['
      pure (read n,reverse x)

-- replace <var>_A^0 with values from variable environment
fillAVars :: [[SomeVar]] -> ValueMap -> Expr -> Expr
fillAVars xss e expr = expr //- [ (allE xs 0,xs') | xs <- nub xss, let xs' = combinedVarsExpr xs ]
  where
    combinedVarsExpr :: [SomeVar] -> Expr
    combinedVarsExpr xs = case sortedEntries xs e of
      Just x -> withValueEntry x (error "fillAVars: something went wrong") (val . map fst)
      Nothing -> error "fillAVars: inconsistent type"
--

instance Arithmetic OutputTerm where
  (.+.) = h2 (.+.) $ value "(+)" ((+) :: Integer -> Integer -> Integer)
  (.-.) = h2 (.-.) $ value "(-)" ((-) :: Integer -> Integer -> Integer)
  (.*.) = h2 (.*.) $ value "(*)" ((*) :: Integer -> Integer -> Integer)
  intLit = Transparent . IntLit . fromInteger

instance Compare OutputTerm where
  (.==.) :: forall a. (Typeable a, Eq a) => OutputTerm a -> OutputTerm a -> OutputTerm Bool
  (.==.) = h2 (.==.) $ value "(==)" ((==) :: a -> a -> Bool)
  (./=.) :: forall a. (Typeable a, Eq a) => OutputTerm a -> OutputTerm a -> OutputTerm Bool
  (./=.) = h2 (./=.) $ value "(/=)" ((/=) :: a -> a -> Bool)
  (.>.) :: forall a. (Typeable a, Ord a) => OutputTerm a -> OutputTerm a -> OutputTerm Bool
  (.>.) =  h2 (.>.) $ value "(>)" ((>) :: a -> a -> Bool)
  (.>=.) :: forall a. (Typeable a, Ord a) => OutputTerm a -> OutputTerm a -> OutputTerm Bool
  (.>=.) =  h2 (.>=.) $ value "(>=)" ((>=) :: a -> a -> Bool)
  (.<.) :: forall a. (Typeable a, Ord a) => OutputTerm a -> OutputTerm a -> OutputTerm Bool
  (.<.) =  h2 (.<.) $ value "(<)" ((<) :: a -> a -> Bool)
  (.<=.) :: forall a. (Typeable a, Ord a) => OutputTerm a -> OutputTerm a -> OutputTerm Bool
  (.<=.) =  h2 (.<=.) $ value "(<=)" ((<=) :: a -> a -> Bool)

instance Logic OutputTerm where
  not' = h1 not' $ value "not" (not :: Bool -> Bool)
  (.&&.) = h2 (.&&.) $ value "(&&)" ((&&) :: Bool -> Bool -> Bool)
  (.||.) = h2 (.||.) $ value "(||)" ((||) :: Bool -> Bool -> Bool)
  true = Transparent $ BoolLit True
  false = Transparent $ BoolLit False

instance Membership OutputTerm where
  isIn = h2 isIn $ value "elem" (elem :: Integer -> [Integer] -> Bool)
  isNotIn = h2 isNotIn $ value "notElem" (notElem :: Integer -> [Integer] -> Bool)

instance BasicLists OutputTerm where
  length' :: forall a. Typeable a => OutputTerm [a] -> OutputTerm Integer
  length' = h1 (length' @ConditionTerm @a) $ value "length" (fromIntegral . length :: [a] -> Integer)

  reverse' :: forall a. Typeable a => OutputTerm [a] -> OutputTerm [a]
  reverse' = h1 (reverse' @ConditionTerm @a) $ value "reverse" (reverse :: [a] -> [a])

  sum' = h1 sum' $ value "sum" (sum :: [Integer] -> Integer)
  product' = h1 product' $ value "product" (product :: [Integer] -> Integer)
  listLit = Transparent . ListLit

h1 :: (ConditionTerm a -> ConditionTerm b) -> Expr -> OutputTerm a -> OutputTerm b
h1 f _ (Transparent t) = Transparent $ f t
h1 _ g (Opaque x vs xs) = Opaque (g :$ x) vs xs

h2 :: (Typeable a, Typeable b) => (ConditionTerm a -> ConditionTerm b -> ConditionTerm c) -> Expr -> OutputTerm a -> OutputTerm b -> OutputTerm c
h2 f _ (Transparent x) (Transparent y) = Transparent $ f x y
h2 _ g (Opaque x vx tx) (Transparent y) = Opaque (g :$ x :$ termExpr y) (vx ++ termVarExps y) (tx ++ subTerms y)
h2 _ g (Transparent x) (Opaque y vy ty) = Opaque (g :$ termExpr x :$ y) (termVarExps x ++ vy) (subTerms x ++ ty)
h2 _ g (Opaque x vx tx) (Opaque y vy ty) = Opaque (g :$ x :$ y) (vx ++ vy) (tx ++ ty)

termExpr :: forall a. Typeable a => ConditionTerm a -> Expr
termExpr (Add x y) = value "(+)" ((+) :: Integer -> Integer -> Integer) :$ termExpr x :$ termExpr y
termExpr (Sub x y) = value "(-)" ((-) :: Integer -> Integer -> Integer) :$ termExpr x :$ termExpr y
termExpr (Mul x y) = value "(*)" ((*) :: Integer -> Integer -> Integer) :$ termExpr x :$ termExpr y
termExpr (Equals x y) = value "(==)" ((==) :: a -> a -> Bool) :$ termExpr x :$ termExpr y
termExpr (Gt x y) = value "(>)" ((>) :: a -> a -> Bool) :$ termExpr x :$ termExpr y
termExpr (Ge x y) = value "(>=)" ((>=) :: a -> a -> Bool) :$ termExpr x :$ termExpr y
termExpr (Lt x y) = value "(<)" ((<) :: a -> a -> Bool) :$ termExpr x :$ termExpr y
termExpr (Le x y) = value "(<=)" ((<=) :: a -> a -> Bool) :$ termExpr x :$ termExpr y
termExpr (And x y) = value "(&&)" ((&&) :: Bool -> Bool -> Bool) :$ termExpr x :$ termExpr y
termExpr (Or x y) = value "(||)" ((||) :: Bool -> Bool -> Bool) :$ termExpr x :$ termExpr y
termExpr (IsIn x xs) = value "elem" (elem :: Integer -> [Integer] -> Bool) :$ termExpr x :$ termExpr xs
termExpr (Not x) = value "not" (not :: Bool -> Bool) :$ termExpr x
termExpr (Length (xs :: ConditionTerm [b])) = value "length" (fromIntegral . length :: [b] -> Integer) :$ termExpr xs
termExpr (Reverse xs) = value "reverse" (reverse :: a -> a) :$ termExpr xs
termExpr (Sum xs) = value "sum" (sum :: [Integer] -> Integer) :$ termExpr xs
termExpr (Product xs) = value "product" (product :: [Integer] -> Integer) :$ termExpr xs
termExpr (IntLit x) = val x
termExpr (ListLit xs) = val xs
termExpr (BoolLit x) = val x
termExpr (Current x n) = currentE x n
termExpr (All x n) = allE x n

instance ComplexLists OutputTerm where
  filter' p (Transparent x) = Opaque (value "filter ?p" (filter p) :$ termExpr x) (termVarExps x) (subTerms x)
  filter' p (Opaque x vs ts) = Opaque (value "filter ?p" (filter p) :$ x) vs ts

instance Opaque OutputTerm where
  liftOpaqueValue (x,str) = Opaque (value str x) [] []

  liftOpaque (f,str) (Transparent x) = Opaque (value str f :$ termExpr x) (termVarExps x) (subTerms x)
  liftOpaque (f,str) (Opaque x vs ts) = Opaque (value str f :$ x) vs ts

  liftOpaque2 (f,str) (Opaque x vx tx) (Opaque y vy ty) = Opaque (value str f :$ x :$ y) (vx ++ vy) (tx ++ ty)
  liftOpaque2 (f,str) (Opaque x vx tx) (Transparent y) = Opaque (value str f :$ x :$ termExpr y) (vx ++ termVarExps y) (tx ++ subTerms y)
  liftOpaque2 (f,str) (Transparent x ) (Opaque y vy ty) = Opaque (value str f :$ termExpr x :$ y) (termVarExps x ++ vy) (subTerms x ++ ty)
  liftOpaque2 (f,str) (Transparent x ) (Transparent y) = Opaque (value str f :$ termExpr x :$ termExpr y) (termVarExps x ++ termVarExps y) (subTerms x ++ subTerms y)
