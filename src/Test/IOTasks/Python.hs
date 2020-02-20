{-# LANGUAGE ViewPatterns #-}
module Test.IOTasks.Python where

import Test.IOTasks.Specification
import Test.IOTasks.Term (AST(..),SynTerm(..), TermVars)

import Text.PrettyPrint
import Data.List ( intercalate )

renderProgramDoc :: Doc -> String
renderProgramDoc = renderStyle (style {lineLength = 1000})

pythonCode :: (SynTerm t, TermVars t) => Specification t -> Doc
pythonCode s = decls $$ body
  where
    decls = Prelude.foldr (\v code -> text (v ++ "_A = []") $$ code) empty (specVars s)
    body = buildPythonProgram s

buildPythonProgram :: SynTerm t => Specification t -> Doc
buildPythonProgram (Spec as) = Prelude.foldr (\a code -> pythonTranslation a $$ code) (text "pass") as

pythonTranslation :: SynTerm t => Action t -> Doc
pythonTranslation (ReadInput x _) = text $ x ++ "_A += [int(input())]"
pythonTranslation (WriteOutput True _ _) = text "pass"
pythonTranslation (WriteOutput False _ (t:_)) = text $ "print(" ++ printPythonCode t ++ ")"
pythonTranslation (WriteOutput False _ _) = error "to time to handle this case correctly"
pythonTranslation (Branch c s1 s2) =
  hang (text ("if " ++ printPythonCode c ++ ":")) 4
    (buildPythonProgram s2)
  $$ hang (text "else:") 4
    (buildPythonProgram s1)
pythonTranslation (TillE s) =
  hang (text "while True:") 4
    (buildPythonProgram s)
pythonTranslation E = text "break"

codeToString :: AST -> String
codeToString (Infix x op y) = codeToString x ++ " " ++ op ++ " " ++ codeToString y
codeToString (Node "length" ts) = "len" ++ "(" ++ intercalate ", " (codeToString <$> ts) ++ ")"
codeToString (Node s ts) = s ++ "(" ++ intercalate ", " (codeToString <$> ts) ++ ")"
codeToString (Leaf (span (/='_') -> (x,"_C"))) = x ++ "_A[-1]"
codeToString (Leaf s) = s
codeToString (Lam _ _) = "<TODO>"

printPythonCode :: SynTerm t => t a -> String
printPythonCode = codeToString . viewTerm
