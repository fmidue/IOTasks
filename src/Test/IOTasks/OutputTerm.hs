{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Test.IOTasks.OutputTerm
  ( OutputTerm
  , SomeOutputTerm(..), withSomeOutputTerm
  , transparentSubterms
  , eval
  ) where

import Prelude hiding (all)

import Test.IOTasks.Terms
import Test.IOTasks.Term hiding (eval)
import qualified Test.IOTasks.Term as Term
import Test.IOTasks.Overflow (OverflowWarning, checkOverflow, OverflowType (..))
import Test.IOTasks.ValueMap

import Data.Express (Expr((:$)), var, val, value, (//-), evl, vars, isVar, showExpr)
import Data.List (nub, intercalate)
import Data.Function (on)

import Type.Reflection
import Data.Kind (Type)
import Type.Match (matchType, fallbackCase', inCaseOfE')

import Text.Parsec (parse, char, many1, alphaNum, sepBy1, (<|>), string, digit)
import Text.Parsec.String (Parser)
import Data.Char (isAlphaNum)

data OutputTerm a
  = Transparent (Term a)
  | Opaque Expr [[Var]] [SomeTerm]

toExpr :: OutputTerm a -> Expr
toExpr (Transparent t) = termExpr t
toExpr (Opaque expr _ _) = expr

transparentSubterms :: Typeable a => OutputTerm a -> [SomeTerm]
transparentSubterms (Transparent t) = subTerms t
transparentSubterms (Opaque _ _ ts) = ts

-- simple instance liftig based on Expr's instances
instance Show (OutputTerm a) where
  show = show . toExpr

instance Eq (OutputTerm a) where
  (==) = (==) `on` toExpr

instance Ord (OutputTerm a) where
  compare = compare `on` toExpr

instance Accessor OutputTerm where
  currentValue' x n = checkNames x $ Transparent $ Current x n
  allValues' x n = checkNames x $ Transparent $ All x n

data SomeOutputTerm where
  SomeOutputTerm :: Typeable a => OutputTerm a -> SomeOutputTerm

withSomeOutputTerm :: SomeOutputTerm -> (forall a. Typeable a => OutputTerm a -> r) -> r
withSomeOutputTerm (SomeOutputTerm t) f = f t

checkNames :: VarExp e => e -> a -> a
checkNames = foldr (f . varname) id . toVarList
  where
    f x c = if legalVar x then c else error $ "illegal variable name: " ++ x ++ "\variable names can only contain letters, digits, _ and '"

legalVar :: String -> Bool
legalVar = any (\c -> isAlphaNum c || c == '_' || c == '\'')

currentE :: VarExp e => e -> Int -> Expr
currentE x n = case varExpType x of
  Just (SomeTypeRep (ty :: TypeRep (a :: k))) -> withTypeable ty $ withTypeable (typeRepKind ty) $
    case eqTypeRep (typeRep @k) (typeRep @Type) of
      Just HRefl -> Data.Express.var ("[" ++ intercalate "," (map varname $ toVarList x) ++ "]_C^" ++ show n) (undefined :: a)
      Nothing -> error $ "currentE: a does not have kind Type in TypeRep a, with a = " ++ show (typeRep @a)
  Nothing -> error "currentE: inconsistent VarExp type"

allE :: VarExp e => e -> Int -> Expr
allE x n = case varExpType x of
  Just (SomeTypeRep (ty :: TypeRep (a :: k))) -> withTypeable ty $ withTypeable (typeRepKind ty) $
    case eqTypeRep (typeRep @k) (typeRep @Type) of
      Just HRefl -> Data.Express.var ("[" ++ intercalate "," (map varname $ toVarList x) ++ "]_A^" ++ show n) (undefined :: [a])
      Nothing -> error $ "allE: a does not have kind Type in TypeRep a, with a = " ++ show (typeRep @a)
  Nothing -> error "allE: inconsistent VarExp type"

eval :: forall a. OverflowType a => OutputTerm a -> ValueMap -> (OverflowWarning, a)
eval (Transparent t) e = Term.eval t e
eval (Opaque expr vss ts) e = let r = eval' expr vss e in matchType @a
  [ inCaseOfE' @Integer $ \HRefl -> (checkOverflow @Integer (fromInteger r),r)
  , fallbackCase' (foldMap (\(SomeTerm t) -> fst $ Term.eval t e) ts,r)]
  where
  eval' :: OverflowType a => Expr -> [[Var]] -> ValueMap -> a
  eval' expr xss e = evl . fillAVars xss e . reduceAVarsIndex e . replaceCVars e $ expr

-- evaluation preprocessing

-- replace <var>_C^n with head(<var>_A^n)
replaceCVars :: ValueMap -> Expr -> Expr
replaceCVars m expr = expr //-
  [ (oldExpr, headF ty :$ allE (varnameVarList x m) n)
  | Just (oldExpr, (n,x)) <- map (varStruct C) (vars expr)
  , let Just ty = varnameTypeRep x m
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
  , let Just ty = varnameTypeRep x m
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

varStruct :: AccessType a -> Expr -> Maybe (Expr,(Int,[Varname]))
varStruct acc x
  | isVar x = either (const Nothing) (Just . (x,)) $ parse (varParser acc) "" (reverse $ showExpr x)
  | otherwise = Nothing
  where
    varParser :: AccessType a -> Parser (Int,[Varname]) -- parses a variable's string representation in reverse
    varParser acc = do
      n <- many1 digit
      _ <- string ("^"++show acc++"_")
      _ <- char ']'
      x <- sepBy1 (many1 (alphaNum <|> char '_' <|> char '\'')) (char ',')
      _ <- char '['
      pure (read n,reverse x)

-- replace <var>_A^0 with values from variable environment
fillAVars :: [[Var]] -> ValueMap -> Expr -> Expr
fillAVars xss e expr = expr //- [ (allE xs 0,xs') | xs <- nub xss, let xs' = combinedVarsExpr xs ]
  where
    combinedVarsExpr :: [Var] -> Expr
    combinedVarsExpr xs = case sortedEntries xs e of
      Just x -> withValueEntry x (error "....ähhh") (val . map fst)
      Nothing -> error "fillAVars: inconsistent type"
--

instance Arithmetic OutputTerm where
  (.+.) = h2 (.+.) $ value "(+)" ((+) :: Integer -> Integer -> Integer)
  (.-.) = h2 (.-.) $ value "(-)" ((-) :: Integer -> Integer -> Integer)
  (.*.) = h2 (.*.) $ value "(*)" ((*) :: Integer -> Integer -> Integer)
  intLit = Transparent . IntLitT . fromInteger

instance BasicLists OutputTerm where
  length' :: forall a. Typeable a => OutputTerm [a] -> OutputTerm Integer
  length' = h1 (length' @Term @a) $ unaryF (Length @a)

  reverse' :: forall a. OverflowType a => OutputTerm [a] -> OutputTerm [a]
  reverse' = h1 (reverse' @Term @a) $ unaryF (Reverse @a)

  sum' = h1 sum' $ unaryF Sum
  product' = h1 product' $ unaryF Product
  listLit = Transparent . ListLitT . toOT

h1 :: (Term a -> Term b) -> Expr -> OutputTerm a -> OutputTerm b
h1 f _ (Transparent t) = Transparent $ f t
h1 _ g (Opaque x vs xs) = Opaque (g :$ x) vs xs

h2 :: (Typeable a, Typeable b) => (Term a -> Term b -> Term c) -> Expr -> OutputTerm a -> OutputTerm b -> OutputTerm c
h2 f _ (Transparent x) (Transparent y) = Transparent $ f x y
h2 _ g (Opaque x vx tx) (Transparent y) = Opaque (g :$ x :$ termExpr y) (vx ++ varExps y) (tx ++ subTerms y)
h2 _ g (Transparent x) (Opaque y vy ty) = Opaque (g :$ termExpr x :$ y) (varExps x ++ vy) (subTerms x ++ ty)
h2 _ g (Opaque x vx tx) (Opaque y vy ty) = Opaque (g :$ x :$ y) (vx ++ vy) (tx ++ ty)

termExpr :: Term a -> Expr
termExpr (termStruct -> Binary f x y) = binF f :$ termExpr x :$ termExpr y
termExpr (termStruct -> Unary f xs) = unaryF f :$ termExpr xs
termExpr (termStruct -> Variable C x n) = currentE x n
termExpr (termStruct -> Variable A x n) = allE x n
termExpr (termStruct -> Literal (BoolLit b)) = val b
termExpr (termStruct -> Literal (IntLit x)) = val x
termExpr (termStruct -> Literal (ListLit xs)) = val $ fromOT xs

unaryF :: forall a b. UnaryF a b -> Expr
unaryF Not = value "not" (not :: Bool -> Bool)
unaryF Length = value "length" (fromIntegral . length :: a -> Integer)
unaryF Reverse = value "reverse" (reverse :: a -> a)
unaryF Sum = value "sum" (sum :: [Integer] -> Integer)
unaryF Product = value "product" (product :: [Integer] -> Integer)

binF :: forall a b c. BinaryF a b c -> Expr
binF (:+:) = value "(+)" ((+) :: Integer -> Integer -> Integer)
binF (:-:) = value "(-)" ((-) :: Integer -> Integer -> Integer)
binF (:*:) = value "(*)" ((*) :: Integer -> Integer -> Integer)
binF (:==:) = value "(==)" ((==) :: a -> a -> Bool)
binF (:>:) = value "(>)" ((>) :: a -> a -> Bool)
binF (:>=:) = value "(>=)" ((>=) :: a -> a -> Bool)
binF (:<:) = value "(<)" ((<) :: a -> a -> Bool)
binF (:<=:) = value "(<=)" ((<=) :: a -> a -> Bool)
binF (:&&:) = value "(&&)" ((&&) :: Bool -> Bool -> Bool)
binF (:||:) = value "(||)" ((||) :: Bool -> Bool -> Bool)
binF IsIn = value "elem" (elem :: Integer -> [Integer] -> Bool)

instance ComplexLists OutputTerm where
  filter' p (Transparent x) = Opaque (value "filter ?p" (filter p) :$ termExpr x) (varExps x) (subTerms x)
  filter' p (Opaque x vs ts) = Opaque (value "filter ?p" (filter p) :$ x) vs ts
