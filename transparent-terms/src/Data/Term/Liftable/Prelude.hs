{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Term.Liftable.Prelude where

import qualified Prelude
import Prelude (Eq, Ord, Foldable, Num, Integral, Bool, Int, (.))
import Data.Term.Liftable

sum :: Liftable term => (Foldable t, Num a) => term (t a) -> term a
sum = liftT (Prelude.sum,"sum")

(+) :: Liftable term => Num a => term a -> term a -> term a
(+) = liftTInfix ((Prelude.+), "+")

(-) :: Liftable term => Num a => term a -> term a -> term a
(-) = liftTInfix ((Prelude.-), "-")

(*) :: Liftable term => Num a => term a -> term a -> term a
(*) = liftTInfix ((Prelude.*), "*")

mod :: Liftable term => Integral a => term a -> term a -> term a
mod = liftT2 (Prelude.mod, "mod")

(==) :: Liftable term => Eq a => term a -> term a -> term Bool
(==) = liftTInfix ((Prelude.==), "==")

(&&) :: Liftable term => term Bool -> term Bool -> term Bool
(&&) = liftTInfix ((Prelude.&&), "&&")

(>) :: Liftable term => Ord a => term a -> term a -> term Bool
(>) = liftTInfix ((Prelude.>), ">")

(<) :: Liftable term => Ord a => term a -> term a -> term Bool
(<) = liftTInfix ((Prelude.<), "<")

nil :: Liftable term => term [a]
nil = embedT ([], "[]") 

cons :: Liftable term => term a -> term [a] -> term [a]
cons = liftTInfix ((:), ":")

(++) :: Liftable term => term [a] -> term [a] -> term [a]
(++) = liftTInfix ((Prelude.++), "++")

length :: Liftable term => Foldable t => term (t a) -> term Int
length = liftT (Prelude.length,"length")

foldr :: Liftable term => Foldable t => (term a -> term b -> term b) -> term b -> term (t a) -> term b
foldr = liftT3 (Prelude.foldr,"foldr") . unHO2

map :: Liftable term => (term a -> term b) -> term [a] -> term [b]
map = liftT2 (Prelude.map, "map") . unHO

filter :: Liftable term => (term a -> term Bool) -> term [a] -> term [a]
filter = liftT2 (Prelude.filter, "filter") . unHO

init :: Liftable term => term [a] -> term [a]
init = liftT (Prelude.init, "init")

last :: Liftable term => term [b] -> term b
last = liftT (Prelude.last, "last")

reverse :: Liftable term => term [a] -> term [a]
reverse = liftT (Prelude.reverse, "reverse")

elem :: Liftable term => Eq a => term a -> term [a] -> term Bool
elem = liftT2 (Prelude.elem,"elem")

notElem :: Liftable term => Eq a => term a -> term [a] -> term Bool
notElem = liftT2 (Prelude.notElem,"notElem")

all :: Liftable term => Foldable t => (term a -> term Bool) -> term (t a) -> term Bool
all = liftT2 (Prelude.all, "all") . unHO

null :: Liftable term => Foldable t => term (t a) -> term Bool
null = liftT (Prelude.null,"null")

not :: Liftable term => term Bool -> term Bool
not = liftT (Prelude.not, "not")
