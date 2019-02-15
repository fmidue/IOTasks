module Language where

type VarName = String

data NumType = IntTy | NatTy | Positive | Negative | Zero | Not NumType deriving (Eq,Ord,Read,Show)

matchesType :: NumType -> Int -> Bool
matchesType IntTy = const True
matchesType NatTy = (>= 0)
matchesType Positive = (> 0)
matchesType Negative = (< 0)
matchesType Zero = (== 0)
matchesType (Not ty) = not . matchesType ty

data Specification
  = ReadInput VarName  NumType  VarName
  | WriteOutput [Function]
  | T
  | Nop
  | TillT Specification
  | Branch Predicate  Specification  Specification
  | Seq Specification  Specification

instance Semigroup Specification where
  (<>) = Seq

data Function
  = UIntF (Int -> Int) VarName
  | BIntF (Int -> Int -> Int) (VarName,VarName)
  | UListF ([Int] -> Int) VarName
  | BListF ([Int] -> [Int] -> Int) (VarName,VarName)
  | MixedF ([Int] -> Int -> Int) (VarName,VarName)

data Predicate
  = UIntP (Int -> Bool) VarName
  | BIntP (Int -> Int -> Bool) (VarName,VarName)
  | UListP ([Int] -> Bool) VarName
  | BListP ([Int] -> [Int] -> Bool) (VarName,VarName)
  | MixedP ([Int] -> Int -> Bool) (VarName,VarName)
