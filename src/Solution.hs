{-# LANGUAGE TupleSections #-}
module Solution where
import Prelude hiding (putStrLn,getLine,print)
import Language
import IOtt

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

buildProgram :: Specification -> IOtt ()
buildProgram s = void $ translate s (newStore s)

translate :: Specification -> Store -> IOtt (Store,LoopEnd)
translate (ReadInput x _ xs) st = do
  v <- read <$> getLine
  return (updateStore st v x xs, No)
translate (WriteOutput []) _ = error "empty list of output options"
translate (WriteOutput (Optional:_)) st = return (st,No)
translate (WriteOutput (f:_)) st = do
  print $ fromMaybe (error "some variable is not is scope") $ evalF f st
  return (st, No)
translate T st = return (st, Yes)
translate Nop st = return (st, No)
translate (TillT s) st =
  let body = translate s
      go st' = do
        (st'', end) <- body st'
        case end of
          Yes -> return (st'', No)
          No -> go st''
  in go st
translate (Branch p s1 s2) st =
  if fromMaybe (error "some variable is not in scope") (evalP p st)
    then translate s2 st
    else translate s1 st
translate (s1 :<> s2) st = translate s1 st >>= (\(st',_) -> translate s2 st')

data LoopEnd = Yes | No

data Store = Store { locals :: [(VarName,Int)], globals :: [(VarName,[Int])] } deriving Show

newStore :: Specification -> Store
newStore s = Store [] ((,[]) <$> globalVars s)

updateStore :: Store -> Int -> VarName -> VarName -> Store
updateStore (Store ls gs) i x xs = Store (update x i ls) (updateWith (:) pure xs i gs)
  where update = updateWith const id
        updateWith :: Eq k => (v1 -> v -> v) -> (v1 -> v) -> k -> v1 -> [(k,v)] -> [(k,v)]
        updateWith f g k v assoc = case foldr (go f k v) (False,[]) assoc of
                                    (False, acc) -> (k, g v) : acc
                                    (True, acc) -> acc
        go :: Eq k => (v1 -> v -> v) -> k -> v1 -> (k,v) -> (Bool,[(k,v)]) -> (Bool,[(k,v)])
        go f k v (k',v') (found,acc) = if k' == k then (True,(k,f v v'):acc) else (found,(k',v'):acc)

evalF :: Function -> Store -> Maybe Int
evalF (UIntF f x) st = f <$> lookup x (locals st)
evalF (BIntF f (x,y)) st = f <$> lookup x (locals st) <*> lookup y (locals st)
evalF (UListF f xs) st = f <$> lookup xs (globals st)
evalF (BListF f (xs,ys)) st = f <$> lookup xs (globals st) <*> lookup ys (globals st)
evalF (MixedF f (xs,y)) st = f <$> lookup xs (globals st) <*> lookup y (locals st)
evalF Optional _ = Nothing

evalP :: Predicate -> Store -> Maybe Bool
evalP (UIntP p x) st = p <$> lookup x (locals st)
evalP (BIntP p (x,y)) st = p <$> lookup x (locals st) <*> lookup y (locals st)
evalP (UListP p xs) st = p <$> lookup xs (globals st)
evalP (BListP p (xs,ys)) st = p <$> lookup xs (globals st) <*> lookup ys (globals st)
evalP (MixedP p (xs,y)) st = p <$> lookup xs (globals st) <*> lookup y (locals st)

globalVars :: Specification -> [VarName]
globalVars = nub . go where
  go (ReadInput _ _ xs) = [xs]
  go (s1 :<> s2) = globalVars s1 ++ globalVars s2
  go (TillT s) = globalVars s
  go (Branch _ s1 s2) = globalVars s1 ++ globalVars s2
  go (WriteOutput _) = []
  go Nop = []
  go T = []
