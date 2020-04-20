module Data.Term.Liftable where

class Liftable t where
  embedT :: (a, String) -> t a
  appT :: t (a -> b) -> t a -> t b

  liftT :: (a -> b, String) -> t a -> t b
  liftT = appT . embedT
  liftT2 :: (a -> b -> c, String) -> t a -> t b -> t c
  liftT2 f = appT . appT (embedT f)
  liftT3 :: (a -> b -> c -> d, String) -> t a -> t b -> t c -> t d
  liftT3 f x = appT . appT (appT (embedT f) x)
  liftTInfix :: (a -> b -> c, String) -> t a -> t b -> t c

  -- | usfull for lifting higher order functions.
  --
  -- E.g. @'liftT2' ('map', "map") . 'unHO'@ lifts @'map'@
  -- to type @(t a -> t b) -> t [a] -> t [b]@
  --
  -- while doing just @'liftT2' ('map', "map")@ would result in type @t (a -> b) -> t [a] -> t [b]@
  unHO :: (t a -> t b) -> t (a -> b)
  unHO2 :: (t a -> t b -> t c) -> t (a -> b -> c)
  unHO3 :: (t a -> t b -> t c -> t d) -> t (a -> b -> c -> d)
