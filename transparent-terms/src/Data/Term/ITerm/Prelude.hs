module Data.Term.ITerm.Prelude where

import qualified Prelude
import Prelude (Eq, Ord, Foldable, Num, Integral, Bool, Int, (.))
import Data.Term.ITerm

sum :: (Foldable t, Num a) => ITerm env v (t a) -> ITerm env v a
sum = liftT (Prelude.sum,"sum")

(+) :: (Eq v, Num a) => ITerm env v a -> ITerm env v a -> ITerm env v a
(+) = liftTInfix ((Prelude.+), "+")

(-) :: (Eq v, Num a) => ITerm env v a -> ITerm env v a -> ITerm env v a
(-) = liftTInfix ((Prelude.-), "-")

(*) :: (Eq v, Num a) => ITerm env v a -> ITerm env v a -> ITerm env v a
(*) = liftTInfix ((Prelude.*), "*")

mod :: (Eq v, Integral a) => ITerm env v a -> ITerm env v a -> ITerm env v a
mod = liftT2 (Prelude.mod, "mod")

(==) :: (Eq v, Eq a) => ITerm env v a -> ITerm env v a -> ITerm env v Bool
(==) = liftTInfix ((Prelude.==), "==")

(&&) :: Eq v => ITerm env v Bool -> ITerm env v Bool -> ITerm env v Bool
(&&) = liftTInfix ((Prelude.&&), "&&")

(>) :: (Eq v, Ord a) => ITerm env v a -> ITerm env v a -> ITerm env v Bool
(>) = liftTInfix ((Prelude.>), ">")

(<) :: (Eq v, Ord a) => ITerm env v a -> ITerm env v a -> ITerm env v Bool
(<) = liftTInfix ((Prelude.<), "<")

length :: Foldable t => ITerm env v (t a) -> ITerm env v Int
length = liftT (Prelude.length,"length")

foldr :: (Eq v, Foldable t) => (ITerm env v a -> ITerm env v b -> ITerm env v b) -> ITerm env v b -> ITerm env v (t a) -> ITerm env v b
foldr = liftT3 (Prelude.foldr,"foldr") . unHO2

map :: Eq v => (ITerm env v a -> ITerm env v b) -> ITerm env v [a] -> ITerm env v [b]
map = liftT2 (Prelude.map, "map") . unHO

filter :: Eq v => (ITerm env v a -> ITerm env v Bool) -> ITerm env v [a] -> ITerm env v [a]
filter = liftT2 (Prelude.filter, "filter") . unHO

init :: Eq v => ITerm env v [a] -> ITerm env v [a]
init = liftT (Prelude.init, "init")

last :: ITerm env v [b] -> ITerm env v b
last = liftT (Prelude.last, "last")

reverse :: ITerm env v [a] -> ITerm env v [a]
reverse = liftT (Prelude.reverse, "reverse")

elem :: (Eq v, Eq a) => ITerm env v a -> ITerm env v [a] -> ITerm env v Bool
elem = liftT2 (Prelude.elem,"elem")

notElem :: (Eq v, Eq a) => ITerm env v a -> ITerm env v [a] -> ITerm env v Bool
notElem = liftT2 (Prelude.notElem,"notElem")

all :: (Eq v, Foldable t) => (ITerm env v a -> ITerm env v Bool) -> ITerm env v (t a) -> ITerm env v Bool
all = liftT2 (Prelude.all, "all") . unHO

null :: Foldable t => ITerm env v (t a) -> ITerm env v Bool
null = liftT (Prelude.null,"null")

not :: ITerm env v Bool -> ITerm env v Bool
not = liftT (Prelude.not, "not")
