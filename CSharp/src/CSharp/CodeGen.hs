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
data Env = Env {
  global :: M.Map Ident Int,
  local :: M.Map Ident Int
} deriving Show
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

insertDecl :: GlobalOrLocal -> Decl -> Env -> Env
insertDecl gol (Decl _ i) env@Env{..}
  | gol == Local = env { local = M.insert i (M.size local + 1) local }
  | otherwise = env { global = M.insert i (M.size global + 1) global }

getDecl :: Ident -> Env -> (GlobalOrLocal, Int)
getDecl i Env{..}
  | M.member i local = (Local, local M.! i)
  | otherwise = (Global, global M.! i)

fClass :: ClassName -> [M] -> C
fClass c ms =
  [LDLA 1, STR R4] ++ gcs ++ [Bsr "main", HALT] ++ lcs
  where
    (_, (gcs, lcs)) = foldl fMethod (Env M.empty M.empty, ([], [])) ms
    fMethod (env, (gcs, lcs)) m =
      let (gol, env', cs) = m env in
      (env',
       if gol == Global
        then (gcs ++ cs, lcs)
        else (gcs, lcs ++ cs ))

fMembDecl :: Decl -> M
fMembDecl d env = (Global, insertDecl Global d env, [AJS 1])

fMembExpr :: E -> M
fMembExpr e env = (Global, env, e env Value)

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s env =
  (Local, env, [LABEL x, LDR MP, LDRR MP SP] ++ map loadParam ps ++ statements ++ [LDRR SP MP,STR MP, RET])
  where
    pl = negate (length ps + 1)
    denv = foldl (flip $ insertDecl Local) env ps
    (senv, statements) = s denv
    loadParam _ = LDS pl

fStatDecl :: Decl -> S
fStatDecl d env = (insertDecl Local d env, [AJS 1])

fStatExpr :: E -> S
fStatExpr e env = (env, e env Value ++ [pop])

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = (env, eCode ++ [BRF (n1 + 2)] ++ snd (s1 env) ++ [BRA n2] ++ snd (s2 env)) where
  eCode        = e env Value
  (n1, n2) = (codeSize (snd $ s1 env), codeSize (snd $ s2 env))

fStatWhile :: E -> S -> S
fStatWhile e s1 env = (env, [BRA n] ++ snd (s1 env) ++ eCode ++ [BRT (-(n + k + 2))]) where
  eCode      = e env Value
  (n, k) = (codeSize (snd $ s1 env), codeSize eCode)

fStatReturn :: E -> S
fStatReturn e env = (env, e env Value ++ [STR R3] ++ [LDRR SP MP,STR MP,RET])

fStatBlock :: [S] -> S
fStatBlock s env = foldl getStat (env, []) s
  where
    getStat (env, cs) s = let (env', cs') = s env in
      (env', cs ++ cs')

fExprLit :: Literal -> E
fExprLit l env va = [LDC value] where
  value = case l of
    LitInt n  -> n
    LitBool b -> bool2int b

codeBool :: Bool -> Env -> Code
codeBool bool env = fExprLit (LitBool bool) env Value

fExprVar :: Ident -> E
fExprVar i env va = case va of
  Value   ->  load $ LDL loc
  Address ->  load $ LDLA loc
  where
    (gol, loc) = getDecl i env
    load code
      | gol == Global = [LDR MP, STR R5, LDRR MP R4, code, LDRR MP R5]
      | otherwise     = [code]

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
fExprCall i es env va = concatMap (\e -> e env Value) es ++ [Bsr i, pop, LDR R3]


-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0