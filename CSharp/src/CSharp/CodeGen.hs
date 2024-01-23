module CSharp.CodeGen where

import           CSharp.AbstractSyntax
import           CSharp.Algebra

import           SSM

import qualified Data.Map              as M
import           Prelude               hiding (EQ, GT, LT)
import Debug.Trace

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type Env = M.Map Ident Int
data GlobalOrLocal = Global | Local
  deriving Eq

type C = Code                              -- Class
type M = Env -> (GlobalOrLocal, Env, Code) -- Member
type S = Env -> (Env, Code)                -- Statement
type E = Env -> (ValueOrAddress -> Code)   -- Expression

codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
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
insertDecl (Decl _ i) env = M.insert i newIndex env
  where
    newIndex = M.size env + 1

getDecl :: Ident -> Env -> Int
getDecl i env = env M.! i

fClass :: ClassName -> [M] -> C
fClass c ms =
  gcs ++ [Bsr "main", HALT] ++ lcs
  where
    (_, (gcs, lcs)) = foldl f (M.empty, ([], [])) ms
    f (env, (gcs, lcs)) m =
      let (gol, env', cs) = m env in
      (M.union env env',
       if gol == Global
        then (gcs ++ cs, lcs)
        else (gcs, lcs ++ cs ))

fMembDecl :: Decl -> M
fMembDecl d env = (Global, insertDecl d env, [])

fMembExpr :: E -> M
fMembExpr e env = (Global, env, e env Value)

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s env = (Local, M.union denv senv, [LABEL x] ++ stackParams ++ statements ++ [RET])
  where
    pl = length ps
    denv = foldr insertDecl env ps
    (senv, statements) = s denv
    f (p, code) (Decl _ i) = (p + 1, code ++ [LDS p, LDLA (getDecl i denv), STA 0])
    stackParams = snd (foldl f (negate pl, []) ps)

fStatDecl :: Decl -> S
fStatDecl d env = (insertDecl d env, [])

fStatExpr :: E -> S
fStatExpr e env = (env, e env Value ++ [pop])

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = (env, c ++ [BRF (n1 + 2)] ++ snd (s1 env) ++ [BRA n2] ++ snd (s2 env)) where
  c        = e env Value
  (n1, n2) = (codeSize (snd $ s1 env), codeSize (snd $ s2 env))

fStatWhile :: E -> S -> S
fStatWhile e s1 env = (env, [BRA n] ++ snd (s1 env) ++ c ++ [BRT (-(n + k + 2))]) where
  c      = e env Value
  (n, k) = (codeSize (snd $ s1 env), codeSize c)

fStatReturn :: E -> S
fStatReturn e env = (env, e env Value ++ [pop] ++ [RET])

fStatBlock :: [S] -> S
fStatBlock s env = foldl f (env, []) s
  where
    f (env, cs) s = let (env', cs') = s env in
      (env', cs ++ cs')

fExprLit :: Literal -> E
fExprLit l env va = [LDC n] where
  n = case l of
    LitInt n  -> n
    LitBool b -> bool2int b

codeBool :: Bool -> Env -> Code
codeBool bool env = fExprLit (LitBool bool) env Value

fExprVar :: Ident -> E
fExprVar i env va = case va of
    Value   ->  [LDL  loc]
    Address ->  [LDLA loc]
  where loc = getDecl i env

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 env va = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
fExprOp OpOr e1 e2 env _ =
  snd $ fStatIf
          e1
          (, codeBool True env)
          (, e2 env Value) env
fExprOp OpAnd e1 e2 env _ =
  snd $ fStatIf
          e1
          (, e2 env Value)
          (, codeBool False env) env
fExprOp op    e1 e2 env va = e1 env Value ++ e2 env Value ++ [
   case op of
    { OpAdd -> ADD;   OpSub -> SUB;   OpMul -> MUL;   OpDiv -> DIV;
      OpMod -> MOD;
      OpXor -> XOR;
      OpLeq -> LE ;   OpLt -> LT ;
      OpGeq -> GT ;   OpGt -> GT ;
      OpEq  -> EQ ;   OpNeq -> NE; }
  ]

fExprCall :: Ident -> [E] -> E
fExprCall "print" es env va = concatMap (\e -> e env Value ++ [TRAP 0]) es ++ [AJS 1]
fExprCall i es env va = concatMap (\e -> e env Value) es ++ [Bsr i, AJS (negate $ length es), LDS (2 + length es)]


-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0