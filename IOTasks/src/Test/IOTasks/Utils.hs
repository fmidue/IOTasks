{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Utils (
  StringEmbedding(..),
  ) where

-- alternative to Show/Read that does not escape strings
class StringEmbedding a where
  pack :: a -> String
  default pack :: Show a => a -> String
  pack = show
  unpack :: String -> a
  default unpack :: Read a => String -> a
  unpack = read

instance {-# OVERLAPPING #-} StringEmbedding String where
  pack = id
  unpack = id

instance StringEmbedding Bool
instance StringEmbedding Int
instance StringEmbedding Integer

instance StringEmbedding a => StringEmbedding [a] where
  pack = show . map pack
  unpack = map unpack . read @[String]
