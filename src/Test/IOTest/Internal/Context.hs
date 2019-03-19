{-# LANGUAGE TupleSections #-}
module Test.IOTest.Internal.Context
  ( Context
  , update
  , freshContext
  , HasVariables(..)
  ) where

type Context v a = [(v,[a])]

update :: Eq v => Context v a -> v -> a -> Context v a
update vs x v = map (x `addValue` v) vs
  where
    addValue x' v' (y,vs') = if y == x' then (y,vs' ++ [v']) else (y,vs')

freshContext :: HasVariables a => a -> Context String s
freshContext s = (,[]) <$> vars s

class HasVariables a where
  vars :: a -> [String]
