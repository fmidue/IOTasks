{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Test.IOTasks.Internal.Term (
  Term(..),
  ConditionTerm,
  OutputTerm,
  TermKind(..),
  oEval,
  evalI, evalIs,
  termVarExps, transparentSubterms,
  toExpr,
  printTerm, printIndexedTerm,
  SomeTerm(..), withSomeTerm,
  castTerm,
  ) where

import Control.Applicative ( liftA2 )

import Data.Express (Expr((:$)), var, val, value, (//-), evl, vars, isVar, showExpr)
import Data.Function (on)
import Data.Kind (Type)
import Data.List ( nub, intercalate, sortBy, intercalate )
import Data.List.Extra (maximumOn)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Maybe ( fromMaybe, mapMaybe, maybeToList )

import Test.IOTasks.Internal.Overflow
import Test.IOTasks.ValueMap as ValueMap
import Test.IOTasks.Var

import Text.Parsec (parse, char, many1, alphaNum, sepBy1, (<|>), string, digit)
import Text.Parsec.String (Parser)

import Type.Match (matchType, fallbackCase', inCaseOfE')
import Type.Reflection

data TermKind = Transparent | PartiallyOpaque

type ConditionTerm = Term 'Transparent
type OutputTerm = Term 'PartiallyOpaque

data Term (k :: TermKind) a where
  Add :: Num a => Term k a -> Term k a -> Term k a
  Sub :: Num a => Term k a -> Term k a -> Term k a
  Mul :: Num a => Term k a -> Term k a -> Term k a
  Equals :: (Typeable a, Eq a) => Term k a -> Term k a -> Term k Bool
  Gt :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool
  Ge :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool
  Lt :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool
  Le :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool
  And :: Term k Bool -> Term k Bool -> Term k Bool
  Or :: Term k Bool -> Term k Bool -> Term k Bool
  IsIn :: Term k Integer -> Term k [Integer] -> Term k Bool
  Not :: Term k Bool -> Term k Bool
  Sum :: Num a => Term k [a] -> Term k a
  Product :: Num a => Term k [a] -> Term k a
  Length :: Typeable a => Term k [a] -> Term k Integer
  Reverse :: Typeable a => Term k [a] -> Term k [a]
  IntLit :: Integer -> Term k Integer
  ListLit :: (Show a, Typeable a) => [a] -> Term k [a]
  BoolLit :: Bool -> Term k Bool
  Current :: VarExp e => e -> Int -> Term k a
  All :: (Typeable a, VarExp e) => e -> Int -> Term k [a]

  Opaque :: Expr -> [[SomeVar]] -> [SomeTerm 'Transparent] -> Term 'PartiallyOpaque a

termVarExps :: Typeable a => Term k a -> [[SomeVar]]
termVarExps (Add x y) = termVarExps x ++ termVarExps y
termVarExps (Sub x y) = termVarExps x ++ termVarExps y
termVarExps (Mul x y) = termVarExps x ++ termVarExps y
termVarExps (Equals x y) = termVarExps x ++ termVarExps y
termVarExps (Gt x y) = termVarExps x ++ termVarExps y
termVarExps (Ge x y) = termVarExps x ++ termVarExps y
termVarExps (Lt x y) = termVarExps x ++ termVarExps y
termVarExps (Le x y) = termVarExps x ++ termVarExps y
termVarExps (And x y) = termVarExps x ++ termVarExps y
termVarExps (Or x y) = termVarExps x ++ termVarExps y
termVarExps (IsIn x y) = termVarExps x ++ termVarExps y
termVarExps (Sum x) = termVarExps x
termVarExps (Product x) = termVarExps x
termVarExps (Length x) = termVarExps x
termVarExps (Reverse x) = termVarExps x
termVarExps (Not x) = termVarExps x
termVarExps (IntLit _) = []
termVarExps (ListLit _) = []
termVarExps (BoolLit _) = []
termVarExps (Current e _) = [toVarList e]
termVarExps (All e _) = [toVarList e]
termVarExps (Opaque _ vars _) = vars

instance EffectEval (Term k) where
  type Env (Term k) = ValueMap
  pureEval f _ (Add x y) = liftA2 (+) (f x) (f y)
  pureEval f _ (Sub x y) = liftA2 (-) (f x) (f y)
  pureEval f _ (Mul x y) = liftA2 (*) (f x) (f y)
  pureEval f _ (Equals x y) = liftA2 (==) (f x) (f y)
  pureEval f _ (Gt x y) = liftA2 (>) (f x) (f y)
  pureEval f _ (Ge x y) = liftA2 (>=) (f x) (f y)
  pureEval f _ (Lt x y) = liftA2 (<) (f x) (f y)
  pureEval f _ (Le x y) = liftA2 (<=) (f x) (f y)
  pureEval f _ (And x y) = liftA2 (&&) (f x) (f y)
  pureEval f _ (Or x y) = liftA2 (||) (f x) (f y)
  pureEval f _ (IsIn x xs) = liftA2 elem (f x) (f xs)
  pureEval f _ (Not x) = not <$> f x
  pureEval f _ (Sum xs) = sum <$> f xs
  pureEval f _ (Product xs) = product <$> f xs
  pureEval f _ (Length xs) = toInteger . length <$> f xs
  pureEval f _ (Reverse xs) = reverse <$> f xs
  pureEval _ _ (IntLit x) = pure x
  pureEval _ _ (ListLit xs) = pure xs
  pureEval _ _ (BoolLit x) = pure x
  pureEval _ e (Current x n) = pure $ fromMaybe (error $ "empty list for {" ++ intercalate "," (map someVarname $ toVarList x) ++ "}") . safeHead $ primEvalVar x n e
  pureEval _ e (All x n) = pure $ reverse $ primEvalVar x n e
  pureEval _ e (Opaque expr vss _) = pure $ eval' expr vss e

oEval :: Typeable a => ValueMap -> Term k a -> (OverflowWarning, a)
oEval = evalOverflow (OverflowTreatment evalI (\d -> Right . evalIs d))

evalI :: ValueMap -> Term k Integer -> Either (SubCheck (Term k) I) I
evalI e (Add x y) = liftA2 (+) (evalI e x) (evalI e y)
evalI e (Sub x y) = liftA2 (-) (evalI e x) (evalI e y)
evalI e (Mul x y) = liftA2 (*) (evalI e x) (evalI e y)
evalI e (Sum xs) = Right $ sum $ evalIs e xs
evalI e (Product xs) = Right $ product $ evalIs e xs
evalI e (Length (xs :: Term k [a])) =
  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Right . fromInt . length $ evalIs e xs
    , fallbackCase' $ Left $ SubCheck xs (fromInt . length)
    ]
evalI _ (IntLit x) = Right $ fromInteger x
evalI e (Current x n) = Right $ fromInteger $ fromMaybe (error $ "empty list for {" ++ intercalate "," (map someVarname $ toVarList x) ++ "}") . safeHead $ primEvalVar x n e
evalI e (Opaque expr vss _) = Right $ fromInteger $ eval' expr vss e

evalIs :: ValueMap -> Term k [Integer] -> [I]
evalIs d (Reverse xs) = reverse $ evalIs d xs
evalIs d (All v n) = map fromInteger $ primEvalVar v n d
evalIs _ (ListLit xs) = map fromInteger xs
evalIs _ Current{} = error "list variables are not supported"
evalIs _ Add{} = error "lists should not have a Num instance"
evalIs _ Sub{} = error "lists should not have a Num instance"
evalIs _ Mul{} = error "lists should not have a Num instance"
evalIs _ Sum{} = error "lists should not have a Num instance"
evalIs _ Product{} = error "lists should not have a Num instance"
evalIs e (Opaque expr vss _ ) = fromInteger <$> eval' expr vss e

primEvalVar :: forall a e. (Typeable a, VarExp e) => e -> Int -> ValueMap -> [a]
primEvalVar x n e =
  drop n . map fst . sortBy (flip compare `on` snd) . concatMap unwrapValueEntry $ mapMaybe (`ValueMap.lookup` e) (toVarList x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

printIndexedTerm :: Typeable a => Term k a -> Map SomeVar (Int,[Int]) -> String
printIndexedTerm t = printTerm' t . Just

printTerm :: Typeable a => Term k a -> String
printTerm t = printTerm' t Nothing

printTerm' :: Typeable a => Term k a -> Maybe (Map SomeVar (Int,[Int])) -> String
printTerm' (Add x y) m = printBinary "+" x y m
printTerm' (Sub x y) m = printBinary "-" x y m
printTerm' (Mul x y) m = printBinary "*" x y m
printTerm' (Equals x y) m = printBinary "==" x y m
printTerm' (Gt x y) m = printBinary ">" x y m
printTerm' (Ge x y) m = printBinary ">=" x y m
printTerm' (Lt x y) m = printBinary "<" x y m
printTerm' (Le x y) m = printBinary "<=" x y m
printTerm' (And x y) m = printBinary "&&" x y m
printTerm' (Or x y) m = printBinary "||" x y m
printTerm' (IsIn x xs) m = printTerm' x m ++ " ∈ " ++ printTerm' xs m
printTerm' (Not (IsIn x xs)) m = printTerm' x m ++ " ∉ " ++ printTerm' xs m
printTerm' (Not t) m = concat ["not (", printTerm' t m, ")"]
printTerm' (BoolLit b) _ = show b
printTerm' (Length xs) m = printUnary "length" xs m
printTerm' (Reverse xs) m = printUnary "reverse" xs m
printTerm' (Sum xs) m = printUnary "sum" xs m
printTerm' (Product xs) m = printUnary "product" xs m
printTerm' (Current x n) (Just m) = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ (\xs -> take (length xs - n) xs) $ mapMaybe (\x -> (someVarname x,) <$> Map.lookup x m) (toVarList x)
printTerm' (Current x n) Nothing = "{" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"++":"++show n++"_C"
printTerm' (All x n) _ = "{" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"++":"++show n++"_A"
printTerm' (IntLit x) _ = show x
printTerm' (ListLit xs) _ = show xs
printTerm' t@Opaque{} _ = show t -- TODO: improve

printBinary :: (Typeable a, Typeable b) => String -> Term k a -> Term k b -> Maybe (Map SomeVar (Int,[Int])) -> String
printBinary op x y m = concat ["(",printTerm' x m, ") ",op," (", printTerm' y m,")"]

printUnary :: Typeable a => String -> Term k a -> Maybe (Map SomeVar (Int,[Int])) -> String
printUnary op x m = concat [op ++" (", printTerm' x m, ")"]

data SomeTerm k where
  SomeTerm :: Typeable a => Term k a -> SomeTerm k
--
someTerm :: Typeable a => Term k a -> SomeTerm k
someTerm = SomeTerm

transparentSubterms :: (Typeable k, Typeable a) => Term k a -> [SomeTerm 'Transparent]
transparentSubterms t@(Add x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Sub x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Mul x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Equals x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Gt x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Ge x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Lt x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Le x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(And x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Or x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(IsIn x y) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x ++ transparentSubterms y
transparentSubterms t@(Sum x) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x
transparentSubterms t@(Product x) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x
transparentSubterms t@(Length x) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x
transparentSubterms t@(Reverse x) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x
transparentSubterms t@(Not x) = maybeToList (someTerm <$> maybeTransparentTerm t) ++ transparentSubterms x
transparentSubterms t@(IntLit _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(ListLit _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(BoolLit _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(Current _ _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(All _ _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms (Opaque _ _ ts) = ts

maybeTransparentTerm :: forall k a. (Typeable k, Typeable a) => Term k a -> Maybe (Term 'Transparent a)
maybeTransparentTerm t = matchType @k
  [ inCaseOfE' @Transparent $ \HRefl -> Just t
  , inCaseOfE' @PartiallyOpaque $ \HRefl ->
      case t of
        Opaque{} -> Nothing
        (Add x y) -> Add <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Sub x y) -> Sub <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Mul x y) -> Mul <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Equals x y) -> Equals <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Gt x y) -> Gt <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Ge x y) -> Ge <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Lt x y) -> Lt <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Le x y) -> Le <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (And x y) -> And <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Or x y) -> Or <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (IsIn x y) -> IsIn <$> maybeTransparentTerm x <*> maybeTransparentTerm y
        (Sum x) -> Sum <$> maybeTransparentTerm x
        (Product x) -> Product <$> maybeTransparentTerm x
        (Length x) -> Length <$> maybeTransparentTerm x
        (Reverse x) -> Reverse <$> maybeTransparentTerm x
        (Not x) -> Not <$> maybeTransparentTerm x
        (IntLit x) -> Just $ IntLit x
        (ListLit xs) -> Just $ ListLit xs
        (BoolLit x) -> Just $ BoolLit x
        (Current x n) -> Just $ Current x n
        (All x n) -> Just $ All x n
  ]

castTerm :: forall {k} a. Typeable a => SomeTerm k -> Maybe (Term k a)
castTerm (SomeTerm (t :: Term k b)) =
  matchType @a
    [ inCaseOfE' @b $ \HRefl -> Just t
    , fallbackCase' Nothing
    ]

-- simple instance lifting based on Expr's instances
instance Typeable a => Show (Term k a) where
  show = show . toExpr

instance Typeable a => Eq (Term k a) where
  (==) = (==) `on` toExpr

instance Typeable a => Ord (Term k a) where
  compare = compare `on` toExpr

withSomeTerm :: SomeTerm k -> (forall a. Typeable a => Term k a -> r) -> r
withSomeTerm (SomeTerm t) f = f t

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

eval' :: Typeable a => Expr -> [[SomeVar]] -> ValueMap -> a
eval' expr xss e = evl . fillAVars xss e . reduceAVarsIndex e . replaceCVars e $ expr

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

toExpr :: forall k a. Typeable a => Term k a -> Expr
toExpr (Add x y) = value "(+)" ((+) :: Integer -> Integer -> Integer) :$ toExpr x :$ toExpr y
toExpr (Sub x y) = value "(-)" ((-) :: Integer -> Integer -> Integer) :$ toExpr x :$ toExpr y
toExpr (Mul x y) = value "(*)" ((*) :: Integer -> Integer -> Integer) :$ toExpr x :$ toExpr y
toExpr (Equals x y) = value "(==)" ((==) :: a -> a -> Bool) :$ toExpr x :$ toExpr y
toExpr (Gt x y) = value "(>)" ((>) :: a -> a -> Bool) :$ toExpr x :$ toExpr y
toExpr (Ge x y) = value "(>=)" ((>=) :: a -> a -> Bool) :$ toExpr x :$ toExpr y
toExpr (Lt x y) = value "(<)" ((<) :: a -> a -> Bool) :$ toExpr x :$ toExpr y
toExpr (Le x y) = value "(<=)" ((<=) :: a -> a -> Bool) :$ toExpr x :$ toExpr y
toExpr (And x y) = value "(&&)" ((&&) :: Bool -> Bool -> Bool) :$ toExpr x :$ toExpr y
toExpr (Or x y) = value "(||)" ((||) :: Bool -> Bool -> Bool) :$ toExpr x :$ toExpr y
toExpr (IsIn x xs) = value "elem" (elem :: Integer -> [Integer] -> Bool) :$ toExpr x :$ toExpr xs
toExpr (Not x) = value "not" (not :: Bool -> Bool) :$ toExpr x
toExpr (Length (xs :: Term k [b])) = value "length" (fromIntegral . length :: [b] -> Integer) :$ toExpr xs
toExpr (Reverse xs) = value "reverse" (reverse :: a -> a) :$ toExpr xs
toExpr (Sum xs) = value "sum" (sum :: [Integer] -> Integer) :$ toExpr xs
toExpr (Product xs) = value "product" (product :: [Integer] -> Integer) :$ toExpr xs
toExpr (IntLit x) = val x
toExpr (ListLit xs) = val xs
toExpr (BoolLit x) = val x
toExpr (Current x n) = currentE x n
toExpr (All x n) = allE x n
toExpr (Opaque expr _ _) = expr