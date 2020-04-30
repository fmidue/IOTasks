{-# LANGUAGE FlexibleContexts #-}
module Test.IOTasks.Python where

import Test.IOTasks.Specification

import Data.Term (SynTerm(..), VarListTerm)
import Data.Term.AST (AST(..))

import Text.PrettyPrint
type Varname = String

renderProgramDoc :: Doc -> String
renderProgramDoc = renderStyle (style {lineLength = 1000})

pythonCode :: (SynTerm t (AST Varname), VarListTerm t Varname) => Specification t -> Doc
pythonCode s = decls $$ body
  where
    decls = Prelude.foldr (\v code -> text (v ++ "_A = []") $$ code) empty (specVars s)
    body = buildPythonProgram s

buildPythonProgram :: SynTerm t (AST Varname) => Specification t -> Doc
buildPythonProgram (Spec as) = Prelude.foldr (\a code -> pythonTranslation a $$ code) (text "pass") as

pythonTranslation :: SynTerm t (AST Varname) => Action (Specification t) t -> Doc
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

codeToString :: AST Varname -> String
codeToString (App (PostApp x (Leaf op)) y) = codeToString x ++ " " ++ op ++ " " ++ codeToString y
codeToString (App (Leaf "length") ts) = "len" ++ "(" ++ codeToString ts ++ ")"
codeToString (App (Leaf s) ts) = s ++ "(" ++ codeToString ts ++ ")"
codeToString (Var x) = x ++ "_A[-1]"
codeToString (VarA x) = x ++ "_A"
codeToString (Leaf s) = s
codeToString (Lam _ _) = "<TODO>"

printPythonCode :: SynTerm t (AST Varname) => t a -> String
printPythonCode = codeToString . viewTerm
