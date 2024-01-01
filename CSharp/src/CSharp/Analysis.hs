module CSharp.Analysis where

import           CSharp.AbstractSyntax
import           CSharp.Algebra

import           SSM

import qualified Data.Map              as M
import           Prelude               hiding (EQ, GT, LT)
import Debug.Trace

type Env = M.Map Ident RetType
data ERet = ERet {
  eIdent   :: Ident,
  eRetType :: RetType
} deriving (Show)

baseERet = ERet {
  eIdent = "",
  eRetType = TyVoid
}

type C = Bool            -- Class
type M = Env -> Env      -- Member
type S = Env -> Env      -- Statement
type E = Env -> ERet     -- Expression

analysisAlgebra :: CSharpAlgebra C M S E
analysisAlgebra = CSharpAlgebra
  fClass
  fMembDecl
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
insertDecl (Decl rt i) env = trace (show env) M.insert i rt env

getDecl :: Ident -> Env -> RetType
getDecl i env
  | M.member i env = env M.! i
  | otherwise = error ("TypeError: " ++ show i ++ " is not defined")

fClass :: ClassName -> [M] -> C
fClass _ ms = mscs `seq` True
  where
    mscs = foldl f M.empty ms
    f env m = let env' = m env in env'

fMembDecl :: Decl -> M
fMembDecl = insertDecl

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth _ i ps s env = M.union denv senv
  where
    denv = foldr insertDecl env ps
    senv = s denv

fStatDecl :: Decl -> S
fStatDecl = insertDecl

fStatExpr :: E -> S
fStatExpr e env = e env `seq` env

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = e env `seq` s1 $ s2 env `seq` env

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

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg address value env = trace ("fExprOp: " ++ show (eRetType $ address env) ++ " " ++ show (eRetType $ value env)) return
  where
    return
      | getDecl (eIdent $ address env) env == eRetType (value env) = baseERet
      | otherwise = error ("TypeError: Variable " ++ eIdent (address env) ++ " expected (" ++ show (eRetType $ address env) ++ ") does not match (" ++ show (eRetType $ value env) ++ ")")
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
fExprCall _ es env =  foldr f baseERet es
  where
    f e baseERet = e env `seq` baseERet


