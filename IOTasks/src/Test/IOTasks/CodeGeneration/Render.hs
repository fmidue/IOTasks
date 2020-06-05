{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
module Test.IOTasks.CodeGeneration.Render where

import Test.IOTasks.CodeGeneration.IR
import Test.IOTasks.CodeGeneration.FreshVar (name)

import Data.Functor.Identity
import Control.Monad.State

import Text.PrettyPrint.HughesPJClass (Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP


data Render = Render
  { renderProg :: Doc -> Doc
  , renderRead :: Var -> Doc
  , renderPrint :: (Doc, Doc) -> Doc -- (argTerm,ctx)
  , renderIf :: (Doc ,Doc) -> Doc -> Doc -> Doc -- (cond,ctx), then, else
  , renderTailCall :: ([Doc], Doc) -> Var -> [Var] -> Doc -- (params,ctx), name, pts -- pts are the names of the parameters at the loop entry
  , renderBindCall :: ([Doc], Doc) -> (Doc, [Var]) -> Var -> [Var] -> Doc -- (params,ctx), (body, pts), name, rvs
  , renderYield :: ([Doc],Doc) -> Doc -- (params,ctx)
  , renderNop :: Doc
  , renderLoop :: Var -> [Var] -> (Doc,Maybe (Doc,Doc)) -> Doc -- f ps (body,Maybe (modBody,exitCond))
  , renderAssignment :: Var -> DefRhs -> Doc

  }

newtype ScopeM a = ScopeM { runScopeM :: [Var] -> (a, [Var]) }
  deriving (Functor, Applicative, Monad, MonadState [Var]) via (StateT [Var] Identity)

evalScopeM :: ScopeM a -> [Var] -> a
evalScopeM m = fst . runScopeM m

haskellCode :: IRProgram -> Doc
haskellCode = renderCode haskellRender

haskellWithReadWriteHoles :: IRProgram -> Doc
haskellWithReadWriteHoles = renderCode haskellRender{ renderRead = hole, renderPrint = hole}
  where hole _ = PP.text "???" 

renderCode :: Render -> IRProgram -> Doc
renderCode r@Render{..} (is,ds,fs) =
    renderProg $ PP.vcat $ evalScopeM (mapM (renderInstruction r ds fs) is) []

renderInstruction :: Render -> [Def] -> [F] -> Instruction -> ScopeM Doc
renderInstruction r@Render{..} ds fs i =
  case i of
    READ x -> return $ renderRead x
    PRINT t -> renderPrint <$> renderVar r ds t
    IF c t e ->
      renderIf
      <$> renderVar r ds c
      <*> (PP.vcat <$> traverse (renderInstruction r ds fs) t)
      <*> (PP.vcat <$> traverse (renderInstruction r ds fs) e)
    TAILCALL f ps ->
      case lookupF f fs of
        Just ((xs,ys),_) -> renderTailCall <$> renderVars r ds ps <*> pure f <*> pure (xs++ys)
        Nothing -> error $ "can't find definition for " ++ name f
    BINDCALL f ps rvs ->
      case lookupF f fs of
        Just ((xs,ys),is) -> do
          condInfo <-
            -- try to detect the loop condition
            case is of
              [IF c t [YIELD _]] -> do
                (cond,_) <- renderVar r ds c
                body <- PP.vcat <$> mapM (renderInstruction r ds fs) t
                return $ Just (body,cond)
              [IF c [YIELD _] e] -> do
                (cond,_) <- renderVar r ds c
                body <- PP.vcat <$> mapM (renderInstruction r ds fs) e
                return $ Just (body,PP.text "!" <> cond)
              _ -> return Nothing
          body <- PP.vcat <$> mapM (renderInstruction r ds fs) is
          renderBindCall
            <$> renderVars r ds ps
            <*> pure (renderLoop f (xs++ys) (body,condInfo), xs++ys)
            <*> pure f
            <*> pure rvs
        Nothing -> error $ "can't find definition for " ++ name f
    YIELD rvs -> renderYield <$> renderVars r ds rvs
    NOP -> return renderNop

renderVars :: Render -> [Def] -> [Var] -> ScopeM ([Doc], Doc)
renderVars _ _ [] = return ([],mempty)
renderVars r ds (v:vs) = do
  (x,ctx) <- renderVar r ds v
  (xs,ctxs) <- renderVars r ds vs
  return (x:xs, ctx PP.$$ ctxs)

-- returns the a Doc for the actual variable and one with the neccessary definitions
renderVar :: Render -> [Def] -> Var -> ScopeM (Doc, Doc)
renderVar r ds x = do
  ctx <- renderContext r ds (neededVars ds x)
  case lookupDef x ds of
    Just (rhs,0,_) -> return (PP.parens (printDefRhs rhs), ctx)
    Just (rhs,1,_) -> return (PP.parens (printDefRhs rhs), ctx)
    Just _ -> return (PP.text $ name x, ctx)
    Nothing -> return (PP.text $ name x, ctx)

neededVars :: [Def] -> Var -> [Var]
neededVars ds x = case lookupDef x ds of
  Just (rhs,_,_) -> let us = map fst (usedVars' [rhs]) in us ++ concatMap (neededVars ds) us
  Nothing -> []

renderContext :: Render -> [Def] -> [Var] -> ScopeM Doc
renderContext Render{..} ds xs = do
  scope <- get
  let notDef = foldr (\x ys -> if x `notElem` scope then maybe ys (\v -> (x,(\(a,_,_) -> a) v):ys) $ lookupDef x ds else ys) [] xs
  return $ PP.hcat $ map (uncurry renderAssignment) notDef

-- rendering to Haskell
haskellRender :: Render
haskellRender =
  let
    renderProg p = PP.text "prog :: IO ()"
      PP.$$ PP.hang (PP.text "prog = do") 2 p
    renderRead x = PP.text $ name x ++ " <- readLn"
    renderPrint (argTerm,ctx) = ctx PP.$$ PP.text "print" PP.<+> argTerm
    renderIf (cond,ctx) thenBranch elseBranch =
      ctx PP.$$ PP.hang (PP.text "if" PP.<+> cond) 2
        (       PP.hang (PP.text "then do") 2 thenBranch
          PP.$$ PP.hang (PP.text "else do") 2 elseBranch
        )
    renderTailCall (params,ctx) f _ = ctx PP.$$ PP.text (name f) PP.<+> PP.hsep params
    renderBindCall (params,ctx) (loopDef,_) f rvs =
      -- loop definition
      loopDef
      -- missing definitions
      PP.$$ ctx
      -- actual call
      PP.$$ (if null rvs then id else (tupelize rvs PP.<+> PP.text "<-" PP.<+>)) (PP.text (name f) PP.<+> PP.hsep params)
    renderYield (params,ctx) = ctx PP.$$ PP.text "return" PP.<+> PP.hsep params
    renderNop = mempty
    renderLoop f ps (body,_) = PP.hang (PP.text $ "let " ++ name f ++ " " ++ unwords (map name ps) ++ " =") 6 body
    renderAssignment x rhs = PP.text ("let " ++ name x ++ " =") PP.<+> printDefRhs rhs
  in Render{..}

-- rendering to imperative pseudo-code
pseudoCode :: IRProgram -> Doc
pseudoCode = renderCode pseudoRender

-- this will produce rather unidiomatic code, due to the variable indexing
-- TODO: WHen is it safe to just drop the indices?
pseudoRender :: Render
pseudoRender =
  let
    renderProg = id
    renderRead x = PP.text $ name x ++ " := input();"
    renderPrint (argTerm,ctx) = ctx PP.$$ PP.text "print" <> PP.parens argTerm <> PP.text ";"
    renderIf (cond,ctx) thenBranch elseBranch =
      PP.vcat
        [ ctx
        , PP.text "if" PP.<+> cond PP.<+> PP.text "{"
        , PP.nest 2 thenBranch
        , PP.text "} else {"
        , PP.nest 2 elseBranch
        , PP.text "}"
        ]
    renderTailCall (ps,ctx) _ pts = PP.vcat $ ctx : zipWith (\p pt -> PP.text (name pt ++ " := ") <> p <> PP.text ";") ps pts
    renderBindCall (ps,ctx) (loopDef,ps') _ rvs =
      PP.vcat
        [ ctx
        , PP.vcat (zipWith (\p p' -> PP.text (name p' ++ " :=") PP.<+> p)  ps ps')
        , loopDef
        , PP.vcat (zipWith (\p' rv -> PP.text (name rv ++ " :=") PP.<+> PP.text (name p')) ps' rvs)
        ]
    renderYield (_,ctx) = ctx PP.$$ PP.text "break;"
    renderNop = mempty
    renderLoop _ _ (_,Just (body,cond)) =
      PP.hang (PP.text "while" PP.<+> cond PP.<+> PP.text " {") 2 body
      PP.$$ PP.text "}"
    renderLoop _ _ (body,_) =
      PP.hang (PP.text "while True {") 2 body
      PP.$$ PP.text "}"
    renderAssignment x rhs = PP.text (name x ++ " :=") PP.<+> printDefRhs rhs
  in Render{..}
