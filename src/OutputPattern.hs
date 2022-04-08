{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module OutputPattern where

import Term
import Data.Maybe (isJust)

import Match as Unification (match)
import qualified Unify as Unification
import Data.List (intersperse)

data OutputPattern (t :: PatternType) where
  Wildcard :: OutputPattern t
  Text :: String -> OutputPattern t
  Sequence :: OutputPattern t -> OutputPattern t -> OutputPattern t
  Value :: Term Integer -> OutputPattern 'SpecificationP

data PatternType = SpecificationP | TraceP

deriving instance Eq (OutputPattern t)
deriving instance Ord (OutputPattern t)
deriving instance Show (OutputPattern t)

instance Semigroup (OutputPattern t) where
  Text "" <> y = y
  x <> Text "" = x
  Text s <> Text t = Text $ s ++ t
  Sequence x y <> z = Sequence x $ y <> z
  x <> y = Sequence x y

instance Monoid (OutputPattern t) where
  mempty = Text ""

evalPattern :: [(Varname,[Integer])] -> OutputPattern t -> OutputPattern 'TraceP
evalPattern _ Wildcard = Wildcard
evalPattern _ (Text s) = Text s
evalPattern e (Sequence x y) = evalPattern e x <> evalPattern e y
evalPattern e (Value t) = Text . show $ eval t e

printPattern :: OutputPattern 'TraceP -> String
printPattern Wildcard = "_"
printPattern (Text s) = s
printPattern (Sequence x y) = printPattern x ++ printPattern y

(>:) :: OutputPattern 'TraceP -> OutputPattern 'TraceP -> Bool
p >: q = isJust $ Unification.match (uTerm p, uTerm q)

uTerm :: OutputPattern 'TraceP -> Unification.Term
uTerm = foldr1 (\t u -> cons [t,u]) . cleanList . termList 1 where
  cons = Unification.term $ Unification.fsym "cons" 2
  charSym c = Unification.constant [c]
  eps = Unification.constant ""
  explicitWS xs = eps : intersperse eps xs ++ [eps]
  toSymList = explicitWS . map charSym
  termList :: Int -> OutputPattern 'TraceP -> [Unification.Term]
  termList i (Sequence Wildcard p') = Unification.var ("wc_" ++ show i) : termList (i+1) p'
  termList i (Sequence (Text s) p') = toSymList s ++ termList i p'
  termList i Wildcard = [Unification.var $ "wc_" ++ show i]
  termList _ (Text s) = toSymList s
  ---
  termList _ (Sequence Sequence{} _) = error "should not happen"
  cleanList :: [Unification.Term] -> [Unification.Term]
  cleanList [] = []
  cleanList [x] = [x]
  cleanList (x:y:ts)
    | Unification.isVar x && y == eps = cleanList (x:ts)
    | x == eps && Unification.isVar y = cleanList (y:ts)
    | x == eps && y == eps = cleanList (x:ts)
    | otherwise = x : cleanList (y:ts)
