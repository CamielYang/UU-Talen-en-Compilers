module CSharp.Analysis where

import           CSharp.AbstractSyntax
import           CSharp.Algebra

import           SSM

import qualified Data.Map              as M
import           Prelude               hiding (EQ, GT, LT)
import Debug.Trace


analysisAlgebra' :: CSharpAlgebra Bool () () ()
analysisAlgebra' = undefinedAlgebra { clas = \_ _ -> True }

undefinedAlgebra :: CSharpAlgebra a b c d
undefinedAlgebra = CSharpAlgebra
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined

type Env = M.Map Ident RetType
data ERet = ERet {
  eIdent   :: Ident,
  eRetType :: RetType,
  eEnv     :: Env
}

baseERet = ERet undefined undefined undefined

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
insertDecl (Decl rt i) env = trace ("insert: " ++ show i) $ M.insert i rt env

getDecl :: Ident -> Env -> RetType
getDecl i env
  | M.member i env = env M.! i
  | otherwise = error ("Variable: " ++ show i ++ " not in scope ")

fClass :: ClassName -> [M] -> C
fClass _ _ = True

fMembDecl :: Decl -> M
fMembDecl = insertDecl

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth _ _ ps _ env = foldr insertDecl env ps

fStatDecl :: Decl -> S
fStatDecl = insertDecl

fStatExpr :: E -> S
fStatExpr _ = id

fStatIf :: E -> S -> S -> S
fStatIf _ _ _ = id

fStatWhile :: E -> S -> S
fStatWhile _ _ = id

fStatReturn :: E -> S
fStatReturn _ = id

fStatBlock :: [S] -> S
fStatBlock s env = foldr ($) env s

fExprLit :: Literal -> E
fExprLit l env = baseERet {
    eRetType = n
}
  where
    n = case l of
      LitInt n  -> NV TyInt
      LitBool b -> NV TyBool

fExprVar :: Ident -> E
fExprVar _ env = baseERet

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg address value env
  | getDecl (eIdent $ address env) env == eRetType (value env) = trace ("test"++ show env) $ baseERet
  | otherwise = error "Type mismatch"
fExprOp _ _ _ env = baseERet

fExprCall :: Ident -> [E] -> E
fExprCall _ _ env = baseERet


