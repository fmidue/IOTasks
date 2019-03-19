module Test.IOTest.Internal.Functions (
  Function(..),
  Predicate(..)
) where

data Function x
  = UIntF (Int -> Int) x
  | BIntF (Int -> Int -> Int) (x,x)
  | UListF ([Int] -> Int) x
  | BListF ([Int] -> [Int] -> Int) (x,x)
  | MixedF ([Int] -> Int -> Int) (x,x)
  | Const Int
  | Optional -- TODO: find a better way to encode epsilon

data Predicate x
  = UIntP (Int -> Bool) x
  | BIntP (Int -> Int -> Bool) (x,x)
  | UListP ([Int] -> Bool) x
  | BListP ([Int] -> [Int] -> Bool) (x,x)
  | MixedP ([Int] -> Int -> Bool) (x,x)

instance Show x => Show (Function x) where
  show (UIntF _ x)      = "*Int -> Int*(" ++ show x ++ ")"
  show (BIntF _ (x,y))  = "*Int -> Int -> Int*(" ++ show x ++ "," ++ show y ++ ")"
  show (UListF _ x)     = "*[Int] -> Int*(" ++ show x ++ ")"
  show (BListF _(x,y))  = "*[Int] -> [Int] -> Int*(" ++ show x ++ "," ++ show y ++ ")"
  show (MixedF _ (x,y)) = "*[Int] -> Int -> Int*(" ++ show x ++ "," ++ show y ++ ")"
  show (Const n) = show n
  show Optional = "\xceb5"

instance Show x => Show (Predicate x) where
  show (UIntP _ x)      = "*Int -> Bool*(" ++ show x ++ ")"
  show (BIntP _ (x,y))  = "*Int -> Int -> Bool*(" ++ show x ++ "," ++ show y ++ ")"
  show (UListP _ x)     = "*[Int] -> Bool*(" ++ show x ++ ")"
  show (BListP _ (x,y)) = "*[Int] -> [Int] -> Bool*(" ++ show x ++ "," ++ show y ++ ")"
  show (MixedP _ (x,y)) = "*[Int] -> Int -> Bool*(" ++ show x ++ "," ++ show y ++ ")"
