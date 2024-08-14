{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTasks.Internal.Term (
  Term(..),
  TermKind(..),
  oEval,
  evalI, evalIs,
  showResult,
  termVarExps, transparentSubterms,
  toExpr,
  showTerm, showIndexedTerm,
  SomeTerm(..), withSomeTerm,
  SomeTermK(..), withSomeTermK,
  castTerm,
  compareK,
  ) where

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

import Type.Match
import Type.Reflection

data TermKind = Transparent | PartiallyOpaque

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
  EmbeddedLit :: (Embeddable a, Typeable a, Show a) => a -> Term k (Embedded a)
  BoolLit :: Bool -> Term k Bool
  Current :: VarExp e => e a -> Int -> Term k a
  All :: (Typeable a, VarExp e) => e a -> Int -> Term k [a]

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
termVarExps (EmbeddedLit _) = []
termVarExps (BoolLit _) = []
termVarExps (Current e _) = [map someVar $ toVarList e]
termVarExps (All e _) = [map someVar $ toVarList e]
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
  pureEval _ _ (EmbeddedLit x) = pure (Embedded $ asInteger x)
  pureEval _ _ (BoolLit x) = pure x
  pureEval _ e (Current x n) = pure $ fromMaybe (error $ "empty list for {" ++ intercalate "," (map varname $ toVarList x) ++ "}") . safeHead $ primEvalVar x n e
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
evalI e (Current x n) = Right $ fromInteger $ fromMaybe (error $ "empty list for {" ++ intercalate "," (map varname $ toVarList x) ++ "}") . safeHead $ primEvalVar x n e
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

primEvalVar :: forall a e. (Typeable a, VarExp e) => e a -> Int -> ValueMap -> [a]
primEvalVar x n e =
  drop n . map fst . sortBy (flip compare `on` snd) . concatMap (unwrapValueEntry (head varList)) $ mapMaybe ((`ValueMap.lookup` e) . someVar) varList
  where
    varList :: [Var a]
    varList = toVarList x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

showResult :: (Show a, Typeable a) => a -> String
showResult x = matchTypeOf x
  [ inCaseOf @String id
  , inCaseOf @Integer show
  , inCaseOfApp @Embedded $ \HRefl (Embedded i :: Embedded a) -> show @a $ asOriginal i
  , fallbackCase' $ show x
  ]

showIndexedTerm :: Typeable a => Term k a -> Map SomeVar (Int,[Int]) -> String
showIndexedTerm t = showTerm' t . Just

showTerm :: Typeable a => Term k a -> String
showTerm t = showTerm' t Nothing

showTerm' :: Typeable a => Term k a -> Maybe (Map SomeVar (Int,[Int])) -> String
showTerm' (Add x y) m = showBinary "+" x y m
showTerm' (Sub x y) m = showBinary "-" x y m
showTerm' (Mul x y) m = showBinary "*" x y m
showTerm' (Equals x y) m = showBinary "==" x y m
showTerm' (Gt x y) m = showBinary ">" x y m
showTerm' (Ge x y) m = showBinary ">=" x y m
showTerm' (Lt x y) m = showBinary "<" x y m
showTerm' (Le x y) m = showBinary "<=" x y m
showTerm' (And x y) m = showBinary "&&" x y m
showTerm' (Or x y) m = showBinary "||" x y m
showTerm' (IsIn x xs) m = showTerm' x m ++ " ∈ " ++ showTerm' xs m
showTerm' (Not (IsIn x xs)) m = showTerm' x m ++ " ∉ " ++ showTerm' xs m
showTerm' (Not t) m = concat ["not (", showTerm' t m, ")"]
showTerm' (BoolLit b) _ = show b
showTerm' (Length xs) m = showUnary "length" xs m
showTerm' (Reverse xs) m = showUnary "reverse" xs m
showTerm' (Sum xs) m = showUnary "sum" xs m
showTerm' (Product xs) m = showUnary "product" xs m
showTerm' (Current x n) (Just m) = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ (\xs -> take (length xs - n) xs) $ mapMaybe (\x -> (varname x,) <$> Map.lookup (someVar x) m) (toVarList x)
showTerm' (Current x n) Nothing = "{" ++ intercalate "," (map varname $ toVarList x) ++ "}"++":"++show n++"_C"
showTerm' (All x n) _ = "{" ++ intercalate "," (map varname $ toVarList x) ++ "}"++":"++show n++"_A"
showTerm' (IntLit x) _ = show x
showTerm' (ListLit xs) _ = show xs
showTerm' (EmbeddedLit x) _ = show x
showTerm' t@Opaque{} _ = show t -- TODO: improve

showBinary :: (Typeable a, Typeable b) => String -> Term k1 a -> Term k2 b -> Maybe (Map SomeVar (Int,[Int])) -> String
showBinary op x y m = concat ["(",showTerm' x m, ") ",op," (", showTerm' y m,")"]

showUnary :: Typeable a => String -> Term k a -> Maybe (Map SomeVar (Int,[Int])) -> String
showUnary op x m = concat [op ++" (", showTerm' x m, ")"]

data SomeTerm k where
  SomeTerm :: Typeable a => Term k a -> SomeTerm k

withSomeTerm :: SomeTerm k -> (forall a. Typeable a => Term k a -> r) -> r
withSomeTerm (SomeTerm t) f = f t

data SomeTermK where
  SomeTermK :: SomeTerm k -> SomeTermK

{- HLINT ignore withSomeTermK "Eta reduce" -}
-- eta-reduced version causes type error in Haddock workflow
withSomeTermK :: SomeTermK -> (forall (k :: TermKind) a. Typeable a => Term k a -> r) -> r
withSomeTermK (SomeTermK t) f = withSomeTerm t f
--
someTerm :: Typeable a => Term k a -> SomeTerm k
someTerm = SomeTerm

transparentSubterms :: Typeable a => Term k a -> [SomeTerm 'Transparent]
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
transparentSubterms t@(EmbeddedLit _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(BoolLit _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(Current _ _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms t@(All _ _) =  maybeToList (someTerm <$> maybeTransparentTerm t)
transparentSubterms (Opaque _ _ ts) = ts

maybeTransparentTerm :: forall (k :: TermKind) a. Typeable a => Term k a -> Maybe (Term 'Transparent a)
maybeTransparentTerm Opaque{} = Nothing
maybeTransparentTerm (Add x y) = Add <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Sub x y) = Sub <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Mul x y) = Mul <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Equals x y) = Equals <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Gt x y) = Gt <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Ge x y) = Ge <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Lt x y) = Lt <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Le x y) = Le <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (And x y) = And <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Or x y) = Or <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (IsIn x y) = IsIn <$> maybeTransparentTerm x <*> maybeTransparentTerm y
maybeTransparentTerm (Sum x) = Sum <$> maybeTransparentTerm x
maybeTransparentTerm (Product x) = Product <$> maybeTransparentTerm x
maybeTransparentTerm (Length x) = Length <$> maybeTransparentTerm x
maybeTransparentTerm (Reverse x) = Reverse <$> maybeTransparentTerm x
maybeTransparentTerm (Not x) = Not <$> maybeTransparentTerm x
maybeTransparentTerm (IntLit x) = Just $ IntLit x
maybeTransparentTerm (ListLit xs) = Just $ ListLit xs
maybeTransparentTerm (EmbeddedLit x) = Just $ EmbeddedLit x
maybeTransparentTerm (BoolLit x) = Just $ BoolLit x
maybeTransparentTerm (Current x n) = Just $ Current x n
maybeTransparentTerm (All x n) = Just $ All x n

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

compareK :: Typeable a => Term k1 a -> Term k2 a -> Ordering
compareK x y = compare (toExpr x) (toExpr y)

currentE ::  forall e a. (VarExp e, Typeable a) => e a -> Int -> Expr
currentE x n =
  Data.Express.var
    ("[" ++ intercalate "," (map varname $ toVarList x) ++ "]_C^" ++ show n)
    (undefined :: a)

allE :: forall e a. (VarExp e, Typeable a) => e a -> Int -> Expr
allE x n =
  Data.Express.var
    ("[" ++ intercalate "," (map varname $ toVarList x) ++ "]_A^" ++ show n)
    (undefined :: [a])

-- Assumption: each list in xss contains variables in ascending order
eval' :: Typeable a => Expr -> [[SomeVar]] -> ValueMap -> a
eval' expr xss e = evl . fillAVars xss e . reduceAVarsIndex e . replaceCVars e $ expr

-- evaluation preprocessing

-- replace <var>_C^n with head(<var>_A^n)
replaceCVars :: ValueMap -> Expr -> Expr
replaceCVars m expr = expr //-
  [ (oldExpr, headF ty :$ withSomeConsistentVars xs ((`allE` n) . merge))
  | Just (oldExpr, (n,x)) <- map (varStruct C) (vars expr)
  , ty <- maybeToList $ varnameTypeRep x m
  , xs <- maybeToList $ varnameVarList x m
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
  [ (expr, tailF ty n :$ withSomeConsistentVars xs ((`allE` 0) . merge))
  | Just (expr, (n,x)) <- map (varStruct A) (vars expr)
  , ty <- maybeToList $ varnameTypeRep x m
  , xs <- maybeToList $ varnameVarList x m
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
  | isVar x = either (const Nothing) (Just . (x,)) $ parse (varParser acc) "" (showExpr x)
  | otherwise = Nothing
  where
    varParser :: AccessType -> Parser (Int,[Varname]) -- parses a variable's string representation
    varParser acc = do
      _ <- char '['
      xs <- sepBy1 (many1 (alphaNum <|> char '_' <|> char '\'')) (char ',')
      _ <- char ']'
      _ <- string ("_"++show acc++"^")
      n <- many1 digit
      pure (read n,xs)

-- replace <var>_A^0 with values from variable environment
fillAVars :: [[SomeVar]] -> ValueMap -> Expr -> Expr
fillAVars xss e expr =
  expr //-
    [ (allExpr,xs')
    | someXs <- nub xss
    , xs <- maybeToList $ someConsistentVars someXs
    , let xs' = withSomeConsistentVars xs combinedVarsExpr
    , let allExpr = withSomeConsistentVars xs ((`allE` 0) . merge)
    ]
  where
    combinedVarsExpr :: Typeable a => [Var a] -> Expr
    combinedVarsExpr xs =
      let x = sortedEntries xs e
      in withValueEntry x (error "fillAVars: something went wrong") (val . map fst)

toExpr :: Typeable a => Term k a -> Expr
toExpr (Add x y) = value "(+)" ((+) :: Integer -> Integer -> Integer) :$ toExpr x :$ toExpr y
toExpr (Sub x y) = value "(-)" ((-) :: Integer -> Integer -> Integer) :$ toExpr x :$ toExpr y
toExpr (Mul x y) = value "(*)" ((*) :: Integer -> Integer -> Integer) :$ toExpr x :$ toExpr y
toExpr (Equals (x :: Term k x) y) = value "(==)" ((==) :: x -> x -> Bool) :$ toExpr x :$ toExpr y
toExpr (Gt (x :: Term k x) y) = value "(>)" ((>) :: x -> x -> Bool) :$ toExpr x :$ toExpr y
toExpr (Ge (x :: Term k x) y) = value "(>=)" ((>=) :: x -> x -> Bool) :$ toExpr x :$ toExpr y
toExpr (Lt (x :: Term k x) y) = value "(<)" ((<) :: x -> x -> Bool) :$ toExpr x :$ toExpr y
toExpr (Le (x :: Term k x) y) = value "(<=)" ((<=) :: x -> x -> Bool) :$ toExpr x :$ toExpr y
toExpr (And x y) = value "(&&)" ((&&) :: Bool -> Bool -> Bool) :$ toExpr x :$ toExpr y
toExpr (Or x y) = value "(||)" ((||) :: Bool -> Bool -> Bool) :$ toExpr x :$ toExpr y
toExpr (IsIn x xs) = value "elem" (elem :: Integer -> [Integer] -> Bool) :$ toExpr x :$ toExpr xs
toExpr (Not x) = value "not" (not :: Bool -> Bool) :$ toExpr x
toExpr (Length (xs :: Term k [b])) = value "length" (fromIntegral . length :: [b] -> Integer) :$ toExpr xs
toExpr (Reverse (xs :: Term k [x])) = value "reverse" (reverse :: [x] -> [x]) :$ toExpr xs
toExpr (Sum xs) = value "sum" (sum :: [Integer] -> Integer) :$ toExpr xs
toExpr (Product xs) = value "product" (product :: [Integer] -> Integer) :$ toExpr xs
toExpr (IntLit x) = val x
toExpr (ListLit xs) = val xs
toExpr (EmbeddedLit (x :: x)) = val @(Embedded x) (Embedded $ asInteger x)
toExpr (BoolLit x) = val x
toExpr (Current x n) = currentE x n
toExpr (All x n) = allE x n
toExpr (Opaque expr _ _) = expr
