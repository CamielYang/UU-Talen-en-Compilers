module CSharp.Analysis.ScopeAnalysis where

import qualified Data.Set as S
import CSharp.AbstractSyntax
import CSharp.Algebra
import CSharp.Analysis.Error
import Data.Either.Validation
import Data.Either
import Debug.Trace

data Phase = Symbol | Analysis deriving (Show, Eq)
type Env   = S.Set Ident

type CRT = Phase -> Env -> C
type C   = Validation [String] Env -- Class
type M   = CRT                     -- Member
type S   = CRT                     -- Statement
type E   = CRT                     -- Expression

scopeAnalysisAlgebra :: CSharpAlgebra C M S E
scopeAnalysisAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembExpr
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprLit
  fExprVar
  fExprOp
  fExprCall

insertDecl :: Decl -> Env -> Env
insertDecl (Decl _ i) = S.insert i

containsDecl :: Ident -> Env -> Bool
containsDecl = S.member

foldGo :: [CRT] -> CRT
foldGo xs phase env = fst $ foldl go (Success env, env) xs
  where
    getEnv :: M -> Phase -> Env -> (C, Env)
    getEnv m phase env = case m phase env of
      Failure err  -> (Failure err, env)
      Success env' -> (Success env', env')

    go :: (C, Env) -> CRT -> (C, Env)
    go (Failure errs, env) m = let (err, env') = getEnv m phase env in (Failure errs <*> err, env')
    go (Success _, env)    m = getEnv m phase env

go :: C -> C -> C
go (Failure errs1) (Failure errs2) = Failure (errs1 <> errs2)
go (Failure errs )  _              = Failure errs
go  _              (Failure errs ) = Failure errs
go (Success env1 ) (Success env2 ) = Success $ env1 `S.union` env2

fClass :: ClassName -> [M] -> C
fClass _ ms = passAnalysis
  where
    passSymbol   = foldGo ms Symbol S.empty
    passAnalysis = foldGo ms Analysis (fromRight S.empty (validationToEither passSymbol))

fMembDecl :: Decl -> M
fMembDecl d Symbol   env = Success $ insertDecl d env
fMembDecl d Analysis env = Success env

fMembExpr :: E -> M
fMembExpr e = e

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth rt i ps s Symbol  env = s Symbol   (insertDecl (Decl rt i) env)
fMembMeth _ _ ps s Analysis env = s Analysis (foldr insertDecl env ps)

fStatDecl :: Decl -> S
fStatDecl d Symbol   env = Success env
fStatDecl d Analysis env = Success $ insertDecl d env

fStatExpr :: E -> S
fStatExpr e = e

fStatIf :: E -> S -> S -> S
fStatIf _ s1 s2 phase env = go (s1 phase env) (s2 phase env)

fStatWhile :: E -> S -> S
fStatWhile _ s = s

fStatReturn :: E -> S
fStatReturn e = e

fStatBlock :: [S] -> S
fStatBlock = foldGo

fExprLit :: Literal -> E
fExprLit _ _ = Success

fExprVar :: Ident -> E
fExprVar i Symbol   env = Success env
fExprVar i Analysis env =
  if containsDecl i env
    then Success env
    else Failure $ scopeError ["Variable", i, "is not in scope"]

fExprOp :: Operator -> E -> E -> E
fExprOp _ e1 e2 phase env = go (e1 phase env) (e2 phase env)

fExprCall :: Ident -> [E] -> E
fExprCall _       args Symbol   env = foldGo args Symbol   env
fExprCall "print" args Analysis env = foldGo args Analysis env
fExprCall i       args Analysis env =
  if containsDecl i env
    then foldGo args Analysis env
    else Failure $ scopeError ["Function", i, "is not in scope"]
