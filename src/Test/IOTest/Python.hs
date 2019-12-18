module Test.IOTest.Python where

import Test.IOTest.Specification
import Test.IOTest.Term
import Test.IOTest.Environment (HasVariables(..))

import Text.PrettyPrint
import Data.List ( intercalate )

renderProgramDoc :: Doc -> String
renderProgramDoc = renderStyle (style {lineLength = 1000})

buildPythonProgram :: (ViewTerm t, TermVars t) => Specification t -> Doc
buildPythonProgram s = decls $$ body
  where
    decls = foldr (\v code -> text (v ++ "_A = []") $$ code) empty (vars s)
    body = buildPythonProgram' s

buildPythonProgram' :: ViewTerm t => Specification t -> Doc
buildPythonProgram' (Spec as) = foldr (\a code -> pythonTranslation a $$ code) (text "pass") as

pythonTranslation :: ViewTerm t => Action t -> Doc
pythonTranslation (ReadInput x _) = text $ x ++ "_A += [int(input())]"
pythonTranslation (WriteOutput True _ _) = text "pass"
pythonTranslation (WriteOutput False _ (t:_)) = text $ "print(" ++ codeToString (viewTerm t) ++ ")"
pythonTranslation (WriteOutput False _ _) = error "to time to handle this case correctly"
pythonTranslation (Branch c s1 s2) =
  hang (text ("if " ++ printTerm c ++ ":")) 4
    (buildPythonProgram' s2)
  $$ hang (text "else:") 4
    (buildPythonProgram' s1)
pythonTranslation (TillE s) =
  hang (text "while True:") 4
    (buildPythonProgram' s)
pythonTranslation E = text "break"

newtype PythonCode a = PyCode { getCode :: AST }

codeToString :: AST -> String
codeToString (Node "==" [x,y]) = codeToString x ++ " == " ++ codeToString y
codeToString (Node s ts) = s ++ "(" ++ intercalate ", " (codeToString <$> ts) ++ ")"
codeToString (Leaf s) = s

printTerm :: ViewTerm t => t a -> String
printTerm = codeToString . viewTerm

instance ViewTerm PythonCode where
  viewTerm = getCode

instance Term PythonCode where
  getAll x = PyCode $ Leaf $ x ++ "_A"
  getCurrent x = PyCode $ Leaf $ x ++ "_A[-1]"

instance LengthTermSym PythonCode where
  len xs = PyCode $ Node "len" [getCode xs]

instance EqTermSym PythonCode where
  eq x y = PyCode $ Node "==" [getCode x, getCode y]

instance SumTermSym PythonCode where
  sum xs = PyCode $ Node "sum" [getCode xs]
