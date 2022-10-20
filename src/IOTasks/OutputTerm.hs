{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module IOTasks.OutputTerm
  ( OutputTerm
  , transparentSubterms
  , eval
  ) where

import Prelude hiding (all)

import IOTasks.Terms
import IOTasks.Term hiding (eval)
import qualified IOTasks.Term as Term
import IOTasks.Overflow (OverflowWarning, checkOverflow)

import Data.Express (Expr((:$)), var, val, value, (//-), evl, vars, isVar, showExpr)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.List (sortBy, nub, intercalate)
import Data.Function (on)
import Data.Maybe (mapMaybe)

import Type.Reflection (Typeable,(:~~:)(..))
import Type.Match (matchType, fallbackCase', inCaseOfE')

import Text.Parsec (parse, char, many1, alphaNum, sepBy1, (<|>), string, digit)
import Text.Parsec.String (Parser)
import Data.Char (isAlphaNum)

data OutputTerm a
  = Transparent (Term a)
  | Opaque Expr [[Varname]] [SomeTerm]

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

checkNames :: VarExp e => e -> a -> a
checkNames = foldr f id . toVarList
  where
    f x c = if legalVarName x then c else error $ "illegal variable name: " ++ x ++ "\variable names can only contain letters, digits, _ and '"

legalVarName :: String -> Bool
legalVarName = any (\c -> isAlphaNum c || c == '_' || c == '\'')

currentE :: VarExp a => a -> Int -> Expr
currentE x n = var ("[" ++ intercalate "," (toVarList x) ++ "]_C^" ++ show n) (undefined :: Integer)

allE :: VarExp a => a -> Int -> Expr
allE x n = var ("[" ++ intercalate "," (toVarList x) ++ "]_A^" ++ show n) (undefined :: [Integer])

eval :: forall a. OverflowType a => OutputTerm a -> Map Varname [(Integer,Int)] -> (OverflowWarning, a)
eval (Transparent t) e = Term.eval t e
eval (Opaque expr vss ts) e = let r = eval' expr vss e in matchType @a
  [ inCaseOfE' @Integer $ \HRefl -> (checkOverflow (fromInteger r),r)
  , fallbackCase' (foldMap (\(SomeTerm t) -> fst $ Term.eval t e) ts,r)]
  where
  eval' :: OverflowType a => Expr -> [[Varname]] -> Map Varname [(Integer,Int)] -> a
  eval' expr xss e = evl . fillAVars xss e . reduceAVarsIndex . replaceCVars $ expr

-- evaluation preprocessing

-- replace <var>_C^n with head(<var>_A^n)
replaceCVars :: Expr -> Expr
replaceCVars expr = expr //- [(expr, value "head" (head :: [Integer] -> Integer) :$ allE x n) | Just (expr, (n,x)) <- map (varStruct C) (vars expr)]

-- replace <var>_A^n with reverse(tail^n(<var>_A^0))
reduceAVarsIndex :: Expr -> Expr
reduceAVarsIndex expr = expr //- [(expr, value ("tail^"++show n) (reverse . replicateF tail n :: [Integer] -> [Integer]) :$ allE x 0) | Just (expr, (n,x)) <- map (varStruct A) (vars expr)]

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
fillAVars :: [[Varname]] -> Map Varname [(Integer,Int)] -> Expr -> Expr
fillAVars xss e expr = expr //- [ (allE xs 0,val xs') | xs <- nub xss, let xs' = combinedVars xs ]
  where
    combinedVars :: [Varname] -> [Integer]
    combinedVars xs = (map fst . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) xs
--

instance Arithmetic OutputTerm where
  (.+.) = h2 (.+.) $ value "(+)" ((+) :: Integer -> Integer -> Integer)
  (.-.) = h2 (.-.) $ value "(-)" ((-) :: Integer -> Integer -> Integer)
  (.*.) = h2 (.*.) $ value "(*)" ((*) :: Integer -> Integer -> Integer)
  intLit = Transparent . IntLitT . fromInteger

instance BasicLists OutputTerm where
  length' = h1 length' $ value "length" (fromIntegral . length :: [Integer] -> Integer)
  sum' = h1 sum' $ value "sum" (sum :: [Integer] -> Integer)
  product' = h1 product' $ value "product" (product :: [Integer] -> Integer)
  listLit = Transparent . ListLitT . map fromInteger

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
termExpr (termStruct -> Var C x n) = currentE x n
termExpr (termStruct -> Var A x n) = allE x n
termExpr (termStruct -> Literal (BoolLit b)) = val b
termExpr (termStruct -> Literal (IntLit x)) = val x
termExpr (termStruct -> Literal (ListLit xs)) = val xs

unaryF :: UnaryF a b -> Expr
unaryF Not = value "not" (not :: Bool -> Bool)
unaryF Length = value "length" (fromIntegral . length :: [Integer] -> Integer)
unaryF Sum = value "sum" (sum :: [Integer] -> Integer)
unaryF Product = value "product" (product :: [Integer] -> Integer)

binF :: BinaryF a b c -> Expr
binF (:+:) = value "(+)" ((+) :: Integer -> Integer -> Integer)
binF (:-:) = value "(-)" ((-) :: Integer -> Integer -> Integer)
binF (:*:) = value "(*)" ((*) :: Integer -> Integer -> Integer)
binF (:==:) = value "(==)" ((==) :: Integer -> Integer -> Bool)
binF (:>:) = value "(>)" ((>) :: Integer -> Integer -> Bool)
binF (:>=:) = value "(>=)" ((>=) :: Integer -> Integer -> Bool)
binF (:<:) = value "(<)" ((<) :: Integer -> Integer -> Bool)
binF (:<=:) = value "(<=)" ((<=) :: Bool -> Bool -> Bool)
binF (:&&:) = value "(&&)" ((&&) :: Bool -> Bool -> Bool)
binF (:||:) = value "(||)" ((||) :: Bool -> Bool -> Bool)
binF IsIn = value "elem" (elem :: Integer -> [Integer] -> Bool)

instance ComplexLists OutputTerm where
  filter' p (Transparent x) = Opaque (value "filter ?p" (filter p) :$ termExpr x) (varExps x) (subTerms x)
  filter' p (Opaque x vs ts) = Opaque (value "filter ?p" (filter p) :$ x) vs ts
