module CSharp.Analysis where

import           CSharp.AbstractSyntax
import           CSharp.Algebra

import           SSM

import qualified Data.Map              as M
import           Prelude               hiding (EQ, GT, LT)
import Debug.Trace

data Compilation = Syntax | Analysis deriving (Show, Eq)

type DEnv = M.Map Ident RetType
type MEnv = M.Map Ident Method

data Method = Method {
  mRetType :: RetType,
  mParams  :: [Decl]
} deriving (Show)

data ERet = ERet {
  eIdent   :: Ident,
  eRetType :: RetType
} deriving (Show)

data Env = Env {
  compilation :: Compilation,
  denv :: DEnv,
  menv :: MEnv
} deriving (Show)

baseERet = ERet {
  eIdent   = "",
  eRetType = TyVoid
}

type C = Bool           -- Class
type M = Env -> Env     -- Member
type S = Env -> Env     -- Statement
type E = Env -> ERet    -- Expression

analysisAlgebra :: CSharpAlgebra C M S E
analysisAlgebra = CSharpAlgebra
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

emptyEnv :: Env
emptyEnv = Env {
  compilation = Syntax,
  denv        = M.empty,
  menv        = M.empty
}

insertDecl :: Decl -> Env -> Env
insertDecl (Decl rt i) env = env { denv = M.insert i rt (denv env) }

getDecl :: Ident -> Env -> RetType
getDecl i Env{..}
  | M.member i denv = denv M.! i
  | otherwise       = error $ unwords ["TypeError:", show i, "is not defined"]

insertMeth :: RetType -> [Decl] -> Ident -> Env -> Env
insertMeth rt ds i env = env { menv = M.insert i (Method rt ds) (menv env) }

getMeth :: Ident -> Env -> Method
getMeth i Env{..}
  | M.member i menv = menv M.! i
  | otherwise       = error $ unwords ["TypeError:", show i,"is not defined"]

fClass :: ClassName -> [M] -> C
fClass _ ms =
  passSyntax   `seq`
  passAnalysis `seq`
  True
  where
    passSyntax   = foldl f (emptyEnv   { compilation = Syntax   }) ms
    passAnalysis = foldl f (passSyntax { compilation = Analysis }) ms
    f env m      = m env

fMembDecl :: Decl -> M
fMembDecl d env@Env{..}
  | compilation == Syntax = insertDecl d env
  | otherwise             = env

fMembExpr :: E -> M
fMembExpr e env = e env `seq` env

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth rt i ps s env@Env{..}
  | compilation == Syntax = s (foldr insertDecl (insertMeth rt ps i env) ps)
  | otherwise             = s env

fStatDecl :: Decl -> S
fStatDecl d env@Env{..}
  | compilation == Syntax = insertDecl d env
  | otherwise             = env

fStatExpr :: E -> S
fStatExpr e env@Env{..} =
  e env `seq`
  env

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env@Env{..} =
  e  env `seq`
  s1 env `seq`
  s2 env `seq`
  env

fStatWhile :: E -> S -> S
fStatWhile e s env@Env{..} =
  e env `seq`
  s env `seq`
  env

fStatReturn :: E -> S
fStatReturn e env@Env{..} =
  e env `seq`
  env

fStatBlock :: [S] -> S
fStatBlock ss env@Env{..} = foldl (\env s -> s env) env ss

fExprLit :: Literal -> E
fExprLit l env = baseERet { eRetType = n }
  where
    n = case l of
      LitInt n  -> NV TyInt
      LitBool b -> NV TyBool

fExprVar :: Ident -> E
fExprVar i env = getDecl i env `seq` baseERet {
  eIdent   = i,
  eRetType = getDecl i env
}

variableError :: Ident -> RetType -> RetType -> Env -> a
variableError v r1 r2 env = error $ unwords [
    "TypeError:"
  , "\"" ++ v ++ "\""
  , "expected"
  , show r1
  , "but got"
  , show r2
  ]

fExprOp :: Operator -> E -> E -> E
fExprOp _ _ _ Env { compilation = Syntax } = baseERet
fExprOp OpAsg address value env
  | addRt == valRt = baseERet { eRetType = addRt }
  | otherwise      = variableError addId addRt valRt env
  where
    addId = eIdent $ address env
    addRt = getDecl addId env
    valRt = eRetType (value env)
fExprOp op e1 e2 env =
  e1 env `seq`
  e2 env `seq`
  baseERet {
    eRetType = case op of {
        OpMul -> NV TyInt ;   OpDiv -> NV TyInt ;   OpMod -> NV TyInt ;
        OpAdd -> NV TyInt ;   OpSub -> NV TyInt ;
        OpLeq -> NV TyBool;   OpLt  -> NV TyBool;   OpGeq -> NV TyBool;   OpGt  -> NV TyBool;
        OpEq  -> NV TyBool;   OpNeq -> NV TyBool;
        OpXor -> NV TyBool;
        OpAnd -> NV TyBool;
        OpOr  -> NV TyBool; }
  }

fExprCall :: Ident -> [E] -> E
fExprCall _ _ e@Env { compilation = Syntax } = baseERet
fExprCall "print" es env
  | checkPrint = foldr f baseERet es
  | otherwise  = error "TypeError: print cannot have void arguments"
  where
    f e baseERet = e env `seq` baseERet
    checkPrint   = all (\e -> eRetType (e env) /= TyVoid) es
fExprCall i es env
  | checkMethod = foldr f (baseERet { eRetType = mRetType }) es
  | otherwise   = error $ unwords [
    "TypeError:"
  , show i
  , "expected"
  , show (length mParams)
  , "arguments, but got"
  , show (length es)
  ]
  where
    Method{..}      = getMeth i env
    f e baseERet    = e env `seq` baseERet
    checkMethod     = length es == length mParams && checkArgs mParams es
    checkArgs [] [] = True
    checkArgs (Decl d di:ds) (e:es)
      | eRetType (e env) == d = checkArgs ds es
      | otherwise             = variableError (i ++ " arg " ++ di) d (eRetType (e env)) env
