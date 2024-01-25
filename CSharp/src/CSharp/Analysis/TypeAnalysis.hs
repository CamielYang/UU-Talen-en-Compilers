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
  eRetType :: RetType
} deriving (Show)

baseERet = ERet {
  eIdent   = "",
  eRetType = TyVoid
}

data VarEnv = VarEnv {
  varName :: Ident,
  varType :: RetType
} deriving (Show)

data MethodEnv = MethodEnv {
  methodName :: Ident,
  methodParams :: [Decl],
  methodType :: RetType
} deriving (Show)

data Phase = Symbol | Analysis deriving (Show, Eq)
data Env   = Env {
  var    :: M.Map Ident VarEnv,
  method :: M.Map Ident MethodEnv
}

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

insertVar :: Decl -> Env -> Env
insertVar (Decl rt i) env@Env{..} =
  env {
    var = M.insert i (VarEnv i rt) var
  }

insertMethod :: Decl -> [Decl] -> Env -> Env
insertMethod (Decl rt i) ps env@Env{..} =
  env {
    method = M.insert i (MethodEnv i ps rt) method
  }

getVar :: Ident -> Env -> VarEnv
getVar i Env{..} = var M.! i

getMethod :: Ident -> Env -> MethodEnv
getMethod i Env{..} = method M.! i

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
go (Success env1 ) (Success env2 ) =
  Success $ env1 {
    var = var env1 `M.union` var env2,
    method = method env1 `M.union` method env2
  }

fClass :: ClassName -> [M] -> C
fClass _ ms = snd passAnalysis
  where
    passSymbol   = snd $ foldGo ms Symbol (Env M.empty M.empty)
    passAnalysis = foldGo ms Analysis (fromRight (Env M.empty M.empty) (validationToEither passSymbol))

fMembDecl :: Decl -> M
fMembDecl d Symbol   env = ([], Success $ insertVar d env)
fMembDecl d Analysis env = ([], Success env)

fMembExpr :: E -> M
fMembExpr e phase env = ([], snd $ e phase env)

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth rt i ps s Symbol env = s Symbol
  (foldr insertVar (insertMethod (Decl rt i) ps env) ps)
fMembMeth rt i ps s Analysis env = case wrongRtType of
  Nothing  -> case sVal of
    Success _ -> (rts, Success env)
    Failure err  -> ([], Failure err)
  Just rt' -> ([], Failure (typeError ["Function", show i, "expected", show rt, "but got", show $ eRetType rt']) <*> sVal)
  where
    wrongRtType = find (\rt' -> rt /= eRetType rt') rts
    s'@(rts, sVal) = s Analysis env

fStatDecl :: Decl -> S
fStatDecl d Symbol   env = ([], Success env)
fStatDecl d Analysis env = ([], Success $ insertVar d env)

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
fExprVar i _ env = ([ERet i varType], Success env)
  where
    VarEnv{..} = getVar i env

fExprOp :: Operator -> E -> E -> E
fExprOp _ _ _ Symbol env = ([], Success env)
fExprOp OpAsg addr val Analysis env =
  if addRt == valRt
    then (fst (addr Analysis env), Success env)
    else (
      [],
      Failure $ typeError ["Can't assign", show valRt, "to", show addRt, "on", show addId]
    )
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
  | otherwise  = (
    [ERet "print" TyVoid],
    Failure $ typeError ["Function print can't be called with void expressions"]
  )
  where
    validPrint = all (\p -> eRetType (head $ fst $ p Analysis env) /= TyVoid) ps
fExprCall i args Analysis env
  | validTypes && validArgsLength = ([ERet methodName methodType], snd $ foldGo args Analysis env)
  | otherwise = ([], Failure $ argErrs <> typeErrs)
  where
    MethodEnv{..} = getMethod i env
    methodps      = methodParams
    methodargs    = map (\p -> eRetType $ head $ fst $ p Analysis env) args
    psZip = zip
      methodps
      methodargs

    -- Arguments
    validArgsLength = length args == length methodps
    argErrs
      | validArgsLength = []
      | otherwise = typeError [
          "Function",
          show i,
          "expected",
          show (length methodps),
          "arguments but got",
          show (length args)]

    -- Types
    validTypes = all (\(Decl r1 _, r2) -> r1 == r2) psZip
    typeErrs   = foldr fTypeErr [] psZip
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