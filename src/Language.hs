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
  = ReadInput VarName NumType
  | WriteOutput [Function]
  | T
  | Nop
  | TillT Specification
  | Branch Predicate Specification Specification
  | Specification :<> Specification
  deriving Show

infixr 6 :<>

instance Semigroup Specification where
  (<>) = (:<>)

data Function
  = UIntF (Int -> Int) VarName
  | BIntF (Int -> Int -> Int) (VarName,VarName)
  | UListF ([Int] -> Int) VarName
  | BListF ([Int] -> [Int] -> Int) (VarName,VarName)
  | MixedF ([Int] -> Int -> Int) (VarName,VarName)
  | Optional -- TODO: find a better way to encode epsilon

data Predicate
  = UIntP (Int -> Bool) VarName
  | BIntP (Int -> Int -> Bool) (VarName,VarName)
  | UListP ([Int] -> Bool) VarName
  | BListP ([Int] -> [Int] -> Bool) (VarName,VarName)
  | MixedP ([Int] -> Int -> Bool) (VarName,VarName)

instance Show Function where
  show (UIntF _ x)      = "*Int -> Int*(" ++ x ++ ")"
  show (BIntF _ (x,y))  = "*Int -> Int -> Int*(" ++ x ++ "," ++ y ++ ")"
  show (UListF _ x)     = "*[Int] -> Int*(" ++ x ++ ")"
  show (BListF _(x,y))  = "*[Int] -> [Int] -> Int*(" ++ x ++ "," ++ y ++ ")"
  show (MixedF _ (x,y)) = "*[Int] -> Int -> Int*(" ++ x ++ "," ++ y ++ ")"
  show Optional = "\xceb5"

instance Show Predicate where
  show (UIntP _ x)      = "*Int -> Bool*(" ++ x ++ ")"
  show (BIntP _ (x,y))  = "*Int -> Int -> Bool*(" ++ x ++ "," ++ y ++ ")"
  show (UListP _ x)     = "*[Int] -> Bool*(" ++ x ++ ")"
  show (BListP _ (x,y)) = "*[Int] -> [Int] -> Bool*(" ++ x ++ "," ++ y ++ ")"
  show (MixedP _ (x,y)) = "*[Int] -> Int -> Bool*(" ++ x ++ "," ++ y ++ ")"
