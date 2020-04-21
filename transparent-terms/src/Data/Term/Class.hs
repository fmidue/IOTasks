{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Term.Class where

import Type.Reflection

-- | terms with variables
class VarTerm t v where
  variable' :: Typeable a => v -> t a

-- | terms with "persistent" variable access
class VarTerm t v => PVarTerm t v where
  variableAll' :: Typeable a => v -> t [a]

variable :: forall a t v. (VarTerm t v, Typeable a) => v -> t a
variable = variable'

variableAll :: forall a t v. (PVarTerm t v, Typeable a) => v -> t [a]
variableAll = variableAll'

-- | alias for variable
variableCurrent :: forall a t v. (Typeable a, PVarTerm t v) => v -> t a
variableCurrent = variable

-- | extract variables used in a term
class VarListTerm t v where
  termVars :: t a -> [v]

isClosed :: (forall v. VarListTerm t v) => t a -> Bool
isClosed = Prelude.null . termVars

-- | terms with extractable usage information
class PVarTerm t v => UsageTerm t v where
  varUsage :: t a ->  [(v,Usage)]

data Usage = Current | All deriving (Show, Eq, Ord, Enum, Bounded)

-- | syntactically inspectable terms
class SynTerm t ast where
  viewTerm :: t a -> ast

class SynTermTyped t ast where
  viewTermTyped :: t a -> ast a

-- | terms with an evaluation semantic over some environemt
class SemTerm t env where
  evalTerm :: t a -> env -> a

-- | terms with an evaluation on closed terms
class ClosedSemTerm t where
  evalClosed :: t a -> a
