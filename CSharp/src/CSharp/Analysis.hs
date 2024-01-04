module CSharp.Analysis where

import           CSharp.AbstractSyntax
import           CSharp.Algebra

import           SSM

import qualified Data.Map              as M
import           Prelude               hiding (EQ, GT, LT)
import Debug.Trace

type DEnv = M.Map Ident RetType
type MEnv = M.Map Ident [Decl]

data ERet = ERet {
  eIdent   :: Ident,
  eRetType :: RetType
} deriving (Show)

data Env = Env {
  denv :: DEnv,
  menv :: MEnv
} deriving (Show)

baseERet = ERet {
  eIdent = "",
  eRetType = TyVoid
}

type C = Bool            -- Class
type M = Env -> Env    -- Member
type S = Env -> Env    -- Statement
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
  denv = M.empty,
  menv = M.empty
}

insertDecl :: Decl -> Env -> Env
insertDecl (Decl rt i) env = trace (show env) $
  env { denv = M.insert i rt (denv env) }

getDecl :: Ident -> Env -> RetType
getDecl i Env{..}
  | M.member i denv = denv M.! i
  | otherwise = error ("TypeError: " ++ show i ++ " is not defined")

insertMeth :: [Decl] -> Ident -> Env -> Env
insertMeth ds i env = env { menv = M.insert i ds (menv env) }

getMeth :: Ident -> Env -> [Decl]
getMeth i Env{..}
  | M.member i menv = menv M.! i
  | otherwise = error ("TypeError: " ++ show i ++ " is not defined")

fClass :: ClassName -> [M] -> C
fClass _ ms = mscs `seq` True
  where
    mscs = foldl f emptyEnv ms
    f env m = let env' = m env in env'

fMembDecl :: Decl -> M
fMembDecl = insertDecl

fMembExpr :: E -> M
fMembExpr e env = e env `seq` env

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth _ i ps s env = senv
  where
    env' = insertMeth ps i env
    decEnv = foldr insertDecl env' ps
    senv = s decEnv

fStatDecl :: Decl -> S
fStatDecl = insertDecl

fStatExpr :: E -> S
fStatExpr e env = e env `seq` env

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = trace "1" e env `seq` s1 $ s2 env `seq` env

fStatWhile :: E -> S -> S
fStatWhile e s env = e env `seq` s env

fStatReturn :: E -> S
fStatReturn e env = e env `seq` env

fStatBlock :: [S] -> S
fStatBlock ss env = foldl (\env s -> s env) env ss

fExprLit :: Literal -> E
fExprLit l env = baseERet { eRetType = n }
  where
    n = case l of
      LitInt n  -> NV TyInt
      LitBool b -> NV TyBool

fExprVar :: Ident -> E
fExprVar i env = getDecl i env `seq` baseERet {
  eIdent = i,
  eRetType = getDecl i env
}

variableError :: Ident -> RetType -> RetType -> Env -> a
variableError v r1 r2 env = error ("TypeError: \"" ++ v ++ "\" expected (" ++ show r1 ++ ") but got (" ++ show r2 ++ ")")

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg address value env = trace ("fExprOp: " ++ show (eRetType $ address env) ++ " " ++ show (eRetType $ value env)) return
  where
    return
      | getDecl (eIdent $ address env) env == eRetType (value env) = baseERet
      | otherwise = variableError (eIdent $ address env) (getDecl (eIdent $ address env) env) (eRetType (value env)) env
fExprOp op _ _ env =
  trace ("fExprOp: " ++ show env) $
  baseERet {
    eRetType = case op of
      {
        OpAdd -> NV TyInt; OpSub -> NV TyInt; OpMul -> NV TyInt; OpDiv -> NV TyInt;
      ; OpMod -> NV TyInt
      ; OpXor -> NV TyBool; OpAnd -> NV TyBool; OpOr  -> NV TyBool;
      ; OpLeq -> NV TyBool; OpLt -> NV TyBool;
      ; OpGeq -> NV TyBool; OpGt -> NV TyBool;
      ; OpEq  -> NV TyBool; OpNeq -> NV TyBool;
      }
  }

fExprCall :: Ident -> [E] -> E
fExprCall i es env = trace ("fExprCall: " ++ show i) result
  where
    result
      | i == "print" || (length es == length ds && checkArgs ds es) = foldr f baseERet es
      | otherwise = error ("TypeError: " ++ show i ++ " expected " ++ show (length ds) ++ " arguments, but got " ++ show (length es))
    ds = getMeth i env
    f e baseERet = e env `seq` baseERet
    checkArgs [] [] = True
    checkArgs (Decl d di:ds) (e:es)
      | eRetType (e env) == d = checkArgs ds es
      | otherwise = variableError (i ++ " arg " ++ di) d (eRetType (e env)) env
