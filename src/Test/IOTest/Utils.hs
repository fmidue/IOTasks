{-# LANGUAGE FlexibleInstances #-}
module Test.IOTest.Utils
  ( (...)
  , StringRep(..)
  ) where

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

class StringRep a where
  to :: a -> String
  from :: String -> a

instance StringRep Int where
  from = read
  to = show

instance StringRep String where
  from = id
  to = id
