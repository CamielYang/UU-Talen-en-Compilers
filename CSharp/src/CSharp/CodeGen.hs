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

type C = Code                            -- Class
type M = Env -> (Env, Code)              -- Member
type S = Env -> (Env, Code)              -- Statement
type E = Env -> (ValueOrAddress -> Code) -- Expression


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
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

insertDecl :: Decl -> Env -> Env
insertDecl (Decl _ i) env
  |  M.member i env = env
  | otherwise = trace ("insert: " ++ show i) $ M.insert i (M.size env + 42) env

fClass :: ClassName -> [M] -> C
fClass c ms = trace ("fClass: " ++ show (length ms)) $ [Bsr "main", HALT] ++ snd mscs
  where
    -- mscs = map (\m -> trace ("fClass map: " ++ show (m M.empty)) $ snd $ m M.empty) ms
    mscs = foldr f (M.empty, []) ms
    f m (env, cs) = trace ("env: " ++ show env') (env', cs' ++ cs)
      where
        (env', cs') = m env

fMembDecl :: Decl -> M
fMembDecl d env = trace ("fMembDecl: " ++ show (env, d)) (insertDecl d env, [])

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s env = trace ("fMembMeth: " ++ show env' ++ show (length ps)) (env', [LABEL x] ++ snd (s env') ++ [RET])
  where
    env' = foldr insertDecl env ps

fStatDecl :: Decl -> S
fStatDecl d env = trace ("fStatDecl: " ++ show (insertDecl d env, d)) (insertDecl d env, [])

fStatExpr :: E -> S
fStatExpr e env = trace ("fStatExpr: " ++ show env) (env, e env Value ++ [pop])

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = trace ("fStatIf: " ++ show env) (env, c ++ [BRF (n1 + 2)] ++ snd (s1 env) ++ [BRA n2] ++ snd (s2 env)) where
  c        = e env Value
  (n1, n2) = (codeSize (snd $ s1 env), codeSize (snd $ s2 env))

fStatWhile :: E -> S -> S
fStatWhile e s1 env = trace ("fStatWhile: " ++ show env) (env, [BRA n] ++ snd (s1 env) ++ c ++ [BRT (-(n + k + 2))]) where
  c = e env Value
  (n, k) = (codeSize (snd $ s1 env), codeSize c)

fStatReturn :: E -> S
fStatReturn e env = trace ("fStatRturn: " ++ show env) (env, e env Value ++ [pop] ++ [RET])

fStatBlock :: [S] -> S
-- fStatBlock s env = trace ("fStatBlock: " ++ show env) (env, concatMap (\x -> snd $ x env) s)
fStatBlock s env = foldl f (env, []) s
  where
    f (env, cs) s = (env', cs ++ cs')
      where
        (env', cs') = s env

fExprLit :: Literal -> E
fExprLit l env va  = trace ("fExprLit: " ++ show env) [LDC n] where
  n = case l of
    LitInt n  -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x env va = trace ("fExprVar: " ++ show (env, x)) $ case va of
    Value   ->  [LDL  loc]
    Address ->  [LDLA loc]
  where loc = env M.! x

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 env va = trace ("fExprOp: " ++ show env) $ e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
fExprOp op    e1 e2 env va = trace ("fExprOp: " ++ show env) $ e1 env Value ++ e2 env Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpAnd -> AND; OpOr -> OR; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0
