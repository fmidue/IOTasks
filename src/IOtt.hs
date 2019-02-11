{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module IOtt where

import Prelude hiding (getLine, putStrLn, print)

import Control.Monad
import Data.Functor.Foldable
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as CMTF
import Data.Deriving (deriveShow1)

data IOtt' t a where
  ReadLine :: (t -> IOtt' t a) -> IOtt' t a
  WriteLine :: t -> IOtt' t a -> IOtt' t a
  Return :: a -> IOtt' t a

instance Show (IOtt' t a) where
  show _ = "TODO: implement Show (IOtt' t a)"

deriving instance Functor (IOtt' t)

instance Applicative (IOtt' t) where
  (<*>) = ap
  pure = Return

instance Monad (IOtt' t) where
  (Return a) >>= g = g a
  (ReadLine f) >>= g = ReadLine (f >=> g)
  (WriteLine s ma) >>= g = WriteLine s (ma >>= g)
  return = pure

type IOtt = IOtt' String

getLine :: IOtt String
getLine = ReadLine Return

putStrLn :: String -> IOtt ()
putStrLn s = WriteLine s $ Return ()

print :: Show a => a -> IOtt ()
print = putStrLn . show

maybePrint :: Show a => Maybe a -> IOtt ()
maybePrint Nothing = return ()
maybePrint (Just xs) = print xs

type Trace' t a = Fix (TraceF t a)
data TraceF t a f
  = ProgReadF t f
  | ProgWriteF t f
  | FinishF a
  | OutOfInputsF
  deriving (Eq, Show,Functor)

type PartialTrace t a = Free (TraceF t a) (IOtt' t a)

changeCarrier :: (Recursive f, Base f ~ TraceF t a) => (t -> t') -> f -> Trace' t' a
changeCarrier f = cata phi where
  phi (FinishF a) = Finish a
  phi (ProgReadF t x) = ProgRead (f t) x
  phi (ProgWriteF t x) = ProgWrite (f t) x
  phi OutOfInputsF = OutOfInputs

pattern ProgRead :: t -> Trace' t a -> Trace' t a
pattern ProgWrite :: t -> Trace' t a -> Trace' t a
pattern Finish :: a -> Trace' t a
pattern OutOfInputs :: Trace' t a
pattern ProgRead t p = Fix (ProgReadF t p)
pattern ProgWrite t p = Fix (ProgWriteF t p)
pattern Finish a = Fix (FinishF a)
pattern OutOfInputs = Fix OutOfInputsF

pattern ProgRead' :: t -> PartialTrace t a -> PartialTrace t a
pattern ProgWrite' :: t -> PartialTrace t a -> PartialTrace t a
pattern Finish' :: a -> PartialTrace t a
pattern OutOfInputs' :: PartialTrace t a
pattern ProgRead' t p = Free (ProgReadF t p)
pattern ProgWrite' t p = Free (ProgWriteF t p)
pattern Finish' a = Free (FinishF a)
pattern OutOfInputs' = Free OutOfInputsF

-- build the trace for the given input and program as far as possible
stepTrace :: t -> IOtt' t a -> PartialTrace t a
stepTrace t prog =
  expandTillRead prog >>= stepWith t >>= expandTillRead
  where
    stepWith :: t -> IOtt' t a -> PartialTrace t a
    stepWith x (ReadLine f) = Free $ ProgReadF x $ Pure (f x)
    stepWith _ _ = error "impossible"

expandTillRead :: IOtt' t a -> PartialTrace t a
expandTillRead (ReadLine f) = Pure $ ReadLine f
expandTillRead (WriteLine x p) = Free $ ProgWriteF x $ expandTillRead p
expandTillRead (Return a) = Free $ FinishF a

runtt :: IOtt' t a -> [t] -> Trace' t a
runtt prog ts =
  let steps = stepTrace <$> ts
      partial = foldM (\p step -> step p) prog steps
  in fromPartial partial

fromPartial :: PartialTrace t a -> Trace' t a
fromPartial = hoist f where
  f :: CMTF.FreeF (TraceF t a) (IOtt' t a) a1 -> TraceF t a a1
  f (CMTF.Pure _) = OutOfInputsF -- if there is a program fragment left the inputs were not enough
  f (CMTF.Free fb) = fb

$(deriveShow1 ''TraceF)
