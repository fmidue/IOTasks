{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
module Types where

import Text.Parsec
import Text.Parsec.String
import Data.Either

data Spec
  = StepSpecs PathSpec
  | Choice [Spec]
  deriving Show

type PathSpec = [(AtomicSpec, Interaction)]

getPaths :: Spec -> [PathSpec]
getPaths (StepSpecs xs) = [xs]
getPaths (Choice xs) = xs >>= getPaths

getCores :: Spec -> [CoreSpec]
getCores = ((fst <$>) <$>) . getPaths

getCore :: PathSpec -> CoreSpec
getCore = fmap fst

type CoreSpec = [AtomicSpec]
data AtomicSpec where
  In :: (VarName, InputType a) -> AtomicSpec
  Out :: (OutputType a,[VarName]) -> AtomicSpec

instance Show AtomicSpec where
  show (In (name, ty)) = mconcat ["In (\"",name,"\",",show ty,")"]
  show (Out (_, xs)) = mconcat ["Out (?, ", show xs, ")"]

type Opt a = Maybe (OptTy,a)
data OptTy = Must | May deriving Show

data Matcher
  = MatchExactly String
  | Template PartialString
  deriving Show

data PartialString
  = Fixed String PartialString
  | Parameter PartialString
  | WhiteSpace PartialString
  | DontCare PartialString
  | Nil
  deriving Show

matchValue :: Matcher
matchValue = Template $ Parameter Nil

printTemplate :: PartialString -> String
printTemplate (Fixed xs r) = xs ++ printTemplate r
printTemplate (DontCare r) = "?" ++ printTemplate r
printTemplate (Parameter r) = "_" ++ printTemplate r
printTemplate (WhiteSpace r) = " " ++ printTemplate r
printTemplate Nil = ""

fillTemplate :: Show a => PartialString -> a -> PartialString
fillTemplate (Fixed xs r) a = Fixed xs $ fillTemplate r a
fillTemplate (Parameter r) a = WhiteSpace $ Fixed (show a) $ WhiteSpace $ fillTemplate r a
fillTemplate (DontCare r) a = DontCare $ fillTemplate r a
fillTemplate (WhiteSpace r) a = WhiteSpace $ fillTemplate r a
fillTemplate Nil _ = Nil

match :: Matcher -> String -> Bool
match (MatchExactly xs) ys = xs == ys
match (Template t) ys =
  isRight $ parse (parseTemplate t) undefined ys

parseTemplate :: PartialString -> Parser ()
parseTemplate (Fixed xs r) = string xs >> parseTemplate r
parseTemplate (WhiteSpace r) = spaces >> parseTemplate r
parseTemplate (DontCare r) = many1 (char '-' <|> alphaNum) >> parseTemplate r
parseTemplate (Parameter _) = fail "unknown parameter"
parseTemplate Nil = eof

outputForm :: Matcher -> String
outputForm (MatchExactly xs) = "String: " ++ show xs
outputForm (Template t) = "String: " ++ printTemplate t

data Interaction
  = OnInput { before :: Opt Matcher
            , feedback :: Opt (Value -> Matcher)
            , after :: Opt Matcher
            }
  | OnOutput { decorate :: Opt Matcher
             }

instance Show Interaction where
  show = const "'some Interaction'"

type VarName = String

data Kind = NonList | List

data BaseType a where
  NumTy :: BaseType Int
  StringTy :: BaseType String

deriving instance Show (BaseType a)

data InputType (p::Kind) where
  Base :: BaseType a -> InputType 'NonList
  SPred :: BaseType a -> (a -> Bool) -> InputType 'NonList
  DPred :: BaseType a -> (a -> a) -> VarName -> InputType 'NonList
  SListTy :: InputType 'NonList -> VarName -> InputType 'List
  DListTy :: InputType 'NonList -> [(VarName,InputType 'NonList)] -> InputType 'List

instance Show (InputType p) where
  show _ = "TODO: implement show for InputType"

intTy :: InputType 'NonList
intTy = Base NumTy

natTy :: InputType 'NonList
natTy = SPred NumTy (>=0)

exact :: Int -> InputType 'NonList
exact n = SPred NumTy (==n)

neg :: VarName -> InputType 'NonList
neg = DPred NumTy negate

data Value
  = IntVal Int
  | IListVal [Int]
  | Line String
  | SListVal [String]

valueToList :: Value -> [Int]
valueToList (IntVal n) = [n]
valueToList (IListVal xs) = xs
valueToList (SListVal _) = error "FIX: change valueToList to handle strings!"
valueToList (Line _) = error "FIX: change valueToList to handle strings!"

fromIntVal :: Value -> Maybe Int
fromIntVal (IntVal i) = Just i
fromIntVal _ = Nothing

fromStringVal :: Value -> Maybe String
fromStringVal (Line xs) = Just xs
fromStringVal _ = Nothing

lookupIntValue :: VarName -> Env -> Maybe Int
lookupIntValue v e = lookup v e >>= fromIntVal

lookupStringValue :: VarName -> Env -> Maybe String
lookupStringValue v e = lookup v e >>= fromStringVal

instance Show Value where
  show (IntVal n) = show n
  show (IListVal xs) = show xs
  show (SListVal xs) = show xs
  show (Line xs) = xs

hasType :: Env -> Value -> InputType a -> Bool
-- integers
hasType _ (IntVal _) (Base NumTy) = True
hasType _ (IntVal i) (SPred NumTy p) = p i
hasType env (IntVal i) (DPred NumTy p vname) =
  lookupIntValue vname env == Just (p i)
hasType env (IListVal xs) (SListTy ty vname) =
  let len = lookupIntValue vname env
      correctLen = Just (length xs) == len
      correctType = map (\x -> hasType env (IntVal x) ty) xs
  in and $ correctLen : correctType
hasType env (IListVal xs) (DListTy ty tys) =
  let correctType = map (\x -> hasType env (IntVal x) ty) xs
      checkSubseq = map (uncurry $ checkSeq env) (slidePattern (IntVal <$> xs) tys)
  in and $ correctType ++ (not <$> init checkSubseq) ++ [last checkSubseq]
-- strings
hasType _ (Line _) (Base StringTy) = True
hasType _ (Line xs) (SPred StringTy p) = p xs
hasType env (Line xs) (DPred StringTy p vname) =
  lookupStringValue vname env == Just (p xs)
hasType env (SListVal xs) (SListTy ty vname) =
  let len = lookupIntValue vname env
      correctLen = Just (length xs) == len
      correctType = map (\x -> hasType env (Line x) ty) xs
  in and $ correctLen : correctType
hasType env (SListVal xs) (DListTy ty tys) =
  let correctType = map (\x -> hasType env (Line x) ty) xs
      checkSubseq = map (uncurry $ checkSeq env) (slidePattern (Line <$> xs) tys)
  in and $ correctType ++ (not <$> init checkSubseq) ++ [last checkSubseq]
-- fallback
hasType _ _ _ = False

checkSeq :: Env -> [Value] -> [(VarName, InputType a)] -> Bool
checkSeq _ [] [] = True
checkSeq _ _ [] = False
checkSeq _ [] _ = False
checkSeq env (v:vs) ((vname,ty):tys)
  | length vs /= length tys = False
  | otherwise = hasType env v ty && checkSeq ((vname,v):env) vs tys

slidePattern :: [a] -> [b] -> [([a],[b])]
slidePattern [] _ = []
slidePattern _ [] = []
slidePattern xs pat
  | length xs >= length pat =
      let ys = take (length pat) xs
      in (ys,pat) : slidePattern (tail xs) pat
  | otherwise = []

data Nat = Z | S Nat
data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat a -> SNat ('S a)

-- pattern sigs cause hlint to fail parsing
-- pattern Unary :: forall (a :: Nat). () => forall (a1 :: Nat).(a~'S a1, a1~'Z) => SNat a
pattern Unary = SS SZ
-- pattern Binary :: forall (a :: Nat). () => forall (a1 :: Nat)(a2 :: Nat).(a~'S a1, a1~'S a2, a2~'Z) => SNat a
pattern Binary = SS Unary

type OutputType a = (SNat a, OutputType' a)

type UnaryOutputType = OutputType ('S 'Z)
type BinaryOutputType = OutputType ('S ('S 'Z))

type family OutputType' (a :: Nat) = result | result -> a where
  OutputType' 'Z = Value
  OutputType' ('S a) = Value -> OutputType' a

sumOf :: UnaryOutputType
sumOf = (Unary, IntVal . sum . valueToList)

append :: BinaryOutputType
append = (Binary,\v1 v2 -> IListVal $ valueToList v1 ++ valueToList v2)

lengthOf :: UnaryOutputType
lengthOf = (Unary, IntVal . length . valueToList)

count :: (Value -> Bool) -> UnaryOutputType
count p = (Unary,\v -> IntVal $ length [ IntVal x | x <- valueToList v, p (IntVal x)])

line :: String -> OutputType 'Z
line xs = (SZ, Line xs)

type Env = [(VarName, Value)]
