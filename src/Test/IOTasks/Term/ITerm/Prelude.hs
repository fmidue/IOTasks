module Test.IOTasks.Term.ITerm.Prelude where

import qualified Prelude
import Prelude (Eq, Ord, Foldable, Num, Integral, Bool, Int, (.))
import Test.IOTasks.Term.ITerm

sum :: (Foldable t, Num a) => ITerm (t a) -> ITerm a
sum = liftT (Prelude.sum,"sum")

(+) :: Num a => ITerm a -> ITerm a -> ITerm a
(+) = liftTInfix ((Prelude.+), "+")

(-) :: Num a => ITerm a -> ITerm a -> ITerm a
(-) = liftTInfix ((Prelude.-), "-")

(*) :: Num a => ITerm a -> ITerm a -> ITerm a
(*) = liftTInfix ((Prelude.*), "*")

mod :: Integral a => ITerm a -> ITerm a -> ITerm a
mod = liftT2 (Prelude.mod, "mod")

(==) :: Eq a => ITerm a -> ITerm a -> ITerm Bool
(==) = liftTInfix ((Prelude.==), "==")

(&&) :: ITerm Bool -> ITerm Bool -> ITerm Bool
(&&) = liftTInfix ((Prelude.&&), "&&")

(>) :: Ord a => ITerm a -> ITerm a -> ITerm Bool
(>) = liftTInfix ((Prelude.>), ">")

(<) :: Ord a => ITerm a -> ITerm a -> ITerm Bool
(<) = liftTInfix ((Prelude.<), "<")

length :: Foldable t => ITerm (t a) -> ITerm Int
length = liftT (Prelude.length,"length")

foldr :: Foldable t => (ITerm a -> ITerm b -> ITerm b) -> ITerm b -> ITerm (t a) -> ITerm b
foldr = liftT3 (Prelude.foldr,"foldr") . unHO2

map :: (ITerm a -> ITerm b) -> ITerm [a] -> ITerm [b]
map = liftT2 (Prelude.map, "map") . unHO

filter :: (ITerm a -> ITerm Bool) -> ITerm [a] -> ITerm [a]
filter = liftT2 (Prelude.filter, "filter") . unHO

init :: ITerm [a] -> ITerm [a]
init = liftT (Prelude.init, "init")

last :: ITerm [b] -> ITerm b
last = liftT (Prelude.last, "last")

reverse :: ITerm [a] -> ITerm [a]
reverse = liftT (Prelude.reverse, "reverse")

elem :: Eq a => ITerm a -> ITerm [a] -> ITerm Bool
elem = liftT2 (Prelude.elem,"elem")

notElem :: Eq a => ITerm a -> ITerm [a] -> ITerm Bool
notElem = liftT2 (Prelude.notElem,"notElem")

all :: Foldable t => (ITerm a -> ITerm Bool) -> ITerm (t a) -> ITerm Bool
all = liftT2 (Prelude.all, "all") . unHO

null :: Foldable t => ITerm (t a) -> ITerm Bool
null = liftT (Prelude.null,"null")

not :: ITerm Bool -> ITerm Bool
not = liftT (Prelude.not, "not")
