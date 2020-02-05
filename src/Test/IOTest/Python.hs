{-# LANGUAGE ViewPatterns #-}
module Test.IOTest.Python where

import Test.IOTest.Specification
import Test.IOTest.Term (Term,AST(..),viewTerm)
import Test.IOTest.Environment (HasVariables(..))

import Text.PrettyPrint
import Data.List ( intercalate )

renderProgramDoc :: Doc -> String
renderProgramDoc = renderStyle (style {lineLength = 1000})

buildPythonProgram :: Specification -> Doc
buildPythonProgram s = decls $$ body
  where
    decls = Prelude.foldr (\v code -> text (v ++ "_A = []") $$ code) empty (vars s)
    body = buildPythonProgram' s

buildPythonProgram' :: Specification -> Doc
buildPythonProgram' (Spec as) = Prelude.foldr (\a code -> pythonTranslation a $$ code) (text "pass") as

pythonTranslation :: Action -> Doc
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

codeToString :: AST -> String
codeToString (Infix x op y) = codeToString x ++ " " ++ op ++ " " ++ codeToString y
codeToString (Node "length" ts) = "len" ++ "(" ++ intercalate ", " (codeToString <$> ts) ++ ")"
codeToString (Node s ts) = s ++ "(" ++ intercalate ", " (codeToString <$> ts) ++ ")"
codeToString (Leaf (span (/='_') -> (x,"_C"))) = x ++ "_A[-1]"
codeToString (Leaf s) = s
codeToString (Lam _ _) = "<TODO>"

printTerm :: Term a -> String
printTerm = codeToString . viewTerm
