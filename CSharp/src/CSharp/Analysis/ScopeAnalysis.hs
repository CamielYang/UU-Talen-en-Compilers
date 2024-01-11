module CSharp.Analysis.ScopeAnalysis where

import qualified Data.Set as S
import CSharp.AbstractSyntax
import CSharp.Algebra
import Data.Either
import Debug.Trace

type Env = S.Set Ident

type C = [String]                     -- Class
type M = Env -> (C, Env)       -- Member
type S = Env -> (C, Env)       -- Statement
type E = Env -> (C, Env)       -- Expression

scopeAnalysisAlgebra :: CSharpAlgebra C M S E
scopeAnalysisAlgebra = CSharpAlgebra {
  clas       = fClass,
  memberD    = fMembDecl,
  memberE    = fMembExpr,
  memberM    = fMembMeth,
  statDecl   = fStatDecl,
  statExpr   = fStatExpr,
  statIf     = fStatIf,
  statWhile  = fStatWhile,
  statReturn = fStatReturn,
  statBlock  = fStatBlock,
  exprLit    = fExprLit,
  exprVar    = fExprVar,
  exprOper   = fExprOp,
  exprCall   = fExprCall
}

insertDecl :: Decl -> Env -> Env
insertDecl (Decl rt i) = S.insert i

getDecl :: Ident -> Env -> Bool
getDecl = S.member

fClass :: ClassName -> [M] -> C
fClass _ ms = fst (foldl go ([], S.empty) ms)
  where
    go :: (C, Env) -> M -> (C, Env)
    go (es, env) m = let (es', env') = m env in (es ++ es', env')

fMembDecl :: Decl -> M
fMembDecl d env = ([], insertDecl d env)

fMembExpr :: E -> M
fMembExpr e = e

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth _ _ ps s env = s (foldr insertDecl env ps)

fStatDecl :: Decl -> S
fStatDecl d env = ([], insertDecl d env)

fStatExpr :: E -> S
fStatExpr e = e

fStatIf :: E -> S -> S -> S
fStatIf _ s1 s2 env = go (s1 env) (s2 env)
  where
    go (es1, env1) (es2, env2) = (es1 ++ es2, S.union env1 env2)

fStatWhile :: E -> S -> S
fStatWhile _ s = s

fStatReturn :: E -> S
fStatReturn e = e

fStatBlock :: [S] -> S
fStatBlock ss env = foldl go ([], env) ss
  where
    go (es, env) s = let (es', env') = s env in (es ++ es', env')

fExprLit :: Literal -> E
fExprLit _ env = ([], env)

fExprVar :: Ident -> E
fExprVar i env =
  if getDecl i env
    then ([], env)
    else ([i ++ " is not in scope"], env)

fExprOp :: Operator -> E -> E -> E
fExprOp _ e1 e2 env = go (e1 env) (e2 env)
  where
    go (es1, env1) (es2, env2) = (es1 ++ es2, S.union env1 env2)

fExprCall :: Ident -> [E] -> E
fExprCall i es env = foldl go ([], env) es
  where
    go (es, env) e = let (es', env') = e env in (es ++ es', env')

