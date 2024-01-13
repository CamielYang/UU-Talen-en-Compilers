module CSharp.Analysis.TypeAnalysis where

import qualified Data.Map as M
import CSharp.AbstractSyntax
import CSharp.Algebra
import CSharp.Analysis.Error
import Data.Either.Validation
import Data.Either
import Debug.Trace
import Data.Maybe
import Data.List

data ERet = ERet {
  eIdent   :: Ident,
  eParams  :: Maybe [Decl],
  eRetType :: RetType
} deriving (Show)

baseERet = ERet {
  eIdent   = "",
  eParams  = Nothing,
  eRetType = TyVoid
}

data Phase = Symbol | Analysis deriving (Show, Eq)
type Env   = M.Map Ident ERet

type CRT = Phase -> Env -> ([ERet], C)
type C   = Validation [String] Env -- Class
type M   = CRT                     -- Member
type S   = CRT                     -- Statement
type E   = CRT                     -- Expression

typeAnalysisAlgebra :: CSharpAlgebra C M S E
typeAnalysisAlgebra = CSharpAlgebra
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

insertDecl :: Decl -> Maybe [Decl] -> Env -> Env
insertDecl (Decl rt i) ps = M.insert i (ERet i ps rt)

getDeclRt :: Ident -> Env -> ERet
getDeclRt i env = env M.! i

foldGo :: [CRT] -> Phase -> Env -> ([ERet], C)
foldGo xs phase env = fst $ foldl go (([], Success env), env) xs
  where
    getEnv :: [ERet] -> M -> Phase -> Env -> (([ERet], C), Env)
    getEnv rt m phase env = let (rt', validation) = m phase env in case validation of
      Failure err  -> ((rt' ++ rt, Failure err), env)
      Success env' -> ((rt' ++ rt, Success env'), env')

    go :: (([ERet], C), Env) -> CRT -> (([ERet], C), Env)
    go ((rt, Failure errs), env) m = let ((rt', err), env') = getEnv rt m phase env in ((rt' ++ rt, Failure errs <*> err), env')
    go ((rt, Success _  ), env) m = getEnv rt m phase env

go :: C -> C -> C
go (Failure errs1) (Failure errs2) = Failure (errs1 <> errs2)
go (Failure errs )  _              = Failure errs
go  _              (Failure errs ) = Failure errs
go (Success env1 ) (Success env2 ) = Success $ env1 `M.union` env2

fClass :: ClassName -> [M] -> C
fClass _ ms = snd passAnalysis
  where
    passSymbol   = snd $ foldGo ms Symbol M.empty
    passAnalysis = foldGo ms Analysis (fromRight M.empty (validationToEither passSymbol))

fMembDecl :: Decl -> M
fMembDecl d Symbol   env = ([], Success $ insertDecl d Nothing env)
fMembDecl d Analysis env = ([], Success env)

fMembExpr :: E -> M
fMembExpr e phase env = ([], snd $ e phase env)

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth rt i ps s Symbol env =
  s Symbol
    (foldr
      (`insertDecl` Nothing)
      (insertDecl (Decl rt i) (Just ps) env)
      ps)
fMembMeth rt i ps s Analysis env = case wrongRtType of
  Nothing -> s'
  Just rt' -> ([], Failure (typeError ["Function", show i, "expected", show rt, "but got", show $ eRetType rt']) <*> sVal)
  where
    wrongRtType = find (\rt' -> rt /= eRetType rt') rts
    s'@(rts, sVal) = s Analysis env

fStatDecl :: Decl -> S
fStatDecl d Symbol   env = ([], Success env)
fStatDecl d Analysis env = ([], Success $ insertDecl d Nothing env)

fStatExpr :: E -> S
fStatExpr e phase env = ([], snd $ e phase env)

fStatIf :: E -> S -> S -> S
fStatIf _ s1 s2 Symbol   env = ([], go (snd $ s1 Symbol env) (snd $ s2 Symbol env))
fStatIf _ s1 s2 Analysis env = (rt1 ++ rt2, go val1 val2)
  where
    (rt1, val1) = s1 Analysis env
    (rt2, val2) = s2 Analysis env

fStatWhile :: E -> S -> S
fStatWhile _ s = s

fStatReturn :: E -> S
fStatReturn e = e

fStatBlock :: [S] -> S
fStatBlock = foldGo

fExprLit :: Literal -> E
fExprLit l _ env = ([baseERet { eRetType = n }], Success env)
  where
    n = case l of
      LitInt  n -> NV TyInt
      LitBool b -> NV TyBool

fExprVar :: Ident -> E
fExprVar i _ env = ([getDeclRt i env], Success env)

fExprOp :: Operator -> E -> E -> E
fExprOp _ _ _ Symbol env = ([], Success env)
fExprOp OpAsg addr val Analysis env =
  if eRetType (head $ fst (addr Analysis env)) == eRetType (head $ fst (val Analysis env))
    then (fst (addr Analysis env), Success env)
    else ([], Failure $ typeError ["Can't assign", show valRt, "to", show addRt, "on", show addId])
  where
    addId = eIdent   $ head $ fst (addr Analysis env)
    addRt = eRetType $ head $ fst (addr Analysis env)
    valRt = eRetType $ head $ fst (val  Analysis env)
fExprOp op e1 e2 _ env =
  ([baseERet {
    eRetType =
      case op of {
        OpMul -> NV TyInt ;   OpDiv -> NV TyInt ;   OpMod -> NV TyInt ;
        OpAdd -> NV TyInt ;   OpSub -> NV TyInt ;
        OpLeq -> NV TyBool;   OpLt  -> NV TyBool;   OpGeq -> NV TyBool;   OpGt  -> NV TyBool;
        OpEq  -> NV TyBool;   OpNeq -> NV TyBool;
        OpXor -> NV TyBool;
        OpAnd -> NV TyBool;
        OpOr  -> NV TyBool;
      }
  }], Success env)

fExprCall :: Ident -> [E] -> E
fExprCall _       ps Symbol   env = foldGo ps Symbol env
fExprCall "print" ps Analysis env
  | validPrint = foldGo ps Analysis env
  | otherwise  = ([ERet "print" Nothing TyVoid], Failure $ typeError ["Function print can't be called with void expressions"])
  where
    validPrint = all (\p -> eRetType (head $ fst $ p Analysis env) /= TyVoid) ps
fExprCall i ps Analysis env
  | validTypes && validArgsLength = ([method], snd $ foldGo ps Analysis env)
  | otherwise = ([], Failure $ argErrs <> typeErrs)
  where
    method = getDeclRt i env
    methodps = fromJust $ eParams method
    psZip = zip
      methodps
      (map (\p -> eRetType $ head $ fst $ p Analysis env) ps)

    -- Arguments
    validArgsLength = length ps == length methodps
    argErrs
      | validArgsLength = []
      | otherwise = typeError [
          "Function",
          show i,
          "expected",
          show (length methodps),
          "arguments but got",
          show (length ps)]

    -- Types
    validTypes = all (\(Decl r1 _, r2) -> r1 == r2) psZip
    typeErrs = foldr fTypeErr [] psZip
    fTypeErr (Decl r1 pi, r2) vs
      | r1 == r2 = vs
      | otherwise = typeError [
          "Function",
          show i,
          "expected",
          show r1,
          "but got",
          show r2,
          "on argument",
          show pi]
      <> vs