module IOTasks.Overflow where

data OverflowWarning = Overflow | NoOverflow deriving (Eq,Show)

instance Semigroup OverflowWarning where
  NoOverflow <> o = o
  o <> NoOverflow = o
  Overflow <> Overflow = Overflow

instance Monoid OverflowWarning where
  mempty = NoOverflow

data I = I Integer Int deriving (Eq,Ord)

instance Show I where
  show i@(I x _)
    | hasDiverged i = show x -- ++ "*"
    | otherwise = show x

hasDiverged :: I -> Bool
hasDiverged (I x x') = x /= fromIntegral x'

checkOverflow :: I -> OverflowWarning
checkOverflow x
  | hasDiverged x = Overflow
  | otherwise = NoOverflow

instance Num I where
  (+) = liftOp2 (+) (+)
  (*) = liftOp2 (*) (*)
  (-) = liftOp2 (-) (-)
  abs = liftOp abs abs
  signum = liftOp signum signum
  fromInteger n = I n (fromInteger n)

liftOp :: (Integer -> Integer) -> (Int -> Int) -> I -> I
liftOp f g (I x x') = I (f x) (g x')

liftOp2 :: (Integer -> Integer -> Integer) -> (Int -> Int -> Int) -> I -> I -> I
liftOp2 f g (I x x') (I y y') = I (f x y) (g x' y')

instance Enum I where
  succ = liftOp succ succO
  pred = liftOp pred predO
  toEnum n = I (toEnum n) (toEnum n)
  fromEnum (I n _) = fromEnum n
  enumFrom x = x : enumFrom (succ x)
  enumFromThen x y = x : f y where
    s = y - x
    f v = v : f (v + s)
  enumFromTo x y =
    case compare x y of
      GT -> []
      EQ -> [x]
      LT -> x : enumFromTo (succ x) y
  enumFromThenTo x y z
    | z < x = []
    | otherwise = x : f y
    where
      s = y - x
      f v | v > z = []
          | otherwise = v : f (v+s)

instance Integral I where
  toInteger (I n _) = n
  quot = liftOp2 quot quot
  rem = liftOp2 rem rem
  div = liftOp2 div div
  mod = liftOp2 mod mod
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)

instance Real I where
  toRational (I n _) = toRational n

succO :: Int -> Int
succO x
  | x == maxBound = minBound
  | otherwise = succ x

predO :: Int -> Int
predO x
  | x == minBound = maxBound
  | otherwise = pred x
