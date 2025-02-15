-- This module defined an abstract syntax tree type for
-- (a subset of) the C# programming language

module CSharp.AbstractSyntax where


type ClassName = String -- Class names
type Ident = String     -- Variable names

data Class    -- Classes (top-level C# programs)
  = Class ClassName [Member]
  deriving (Eq, Ord, Show)

data Member   -- Class members
  = MemberD Decl                      -- global variable declaration
  | MemberE Expr                      -- global expression
  | MemberM RetType Ident [Decl] Stat -- function (aka "method") defintions
  deriving (Eq, Ord, Show)

data Stat     -- Statements
  = StatDecl   Decl
  | StatExpr   Expr
  | StatIf     Expr Stat Stat
  | StatWhile  Expr Stat
  | StatReturn Expr
  | StatBlock  [Stat]
  deriving (Eq, Ord, Show)

data Literal = LitInt Int | LitBool Bool
  deriving (Eq, Ord, Show)

data Expr   -- Expressions
  = ExprLit   Literal
  | ExprVar   Ident
  | ExprOper  Operator Expr Expr
  | ExprCall  Ident [Expr]
  deriving (Eq, Ord, Show)

data Operator -- Binary operators
  = OpMul | OpDiv | OpMod       -- Multiplicative
  | OpAdd | OpSub               -- Additive
  | OpLeq | OpLt | OpGeq | OpGt -- Relational and type-testing
  | OpEq  | OpNeq               -- Equality
  | OpXor                       -- Conditional XOR
  | OpAnd                       -- Conditional AND
  | OpOr                        -- Conditional OR
  | OpAsg                       -- Assignment
  deriving (Eq, Show, Ord, Enum, Bounded)

data Decl = Decl RetType Ident  -- Variable declarations
  deriving (Eq, Ord, Show)

-- (Simplified) types of C#, not including "void"
data Type = TyBool | TyInt
  deriving (Eq, Ord, Enum, Bounded)

-- (Simplified) types of C#, including "void"
data RetType
  = TyVoid      -- "void"
  | NV Type     -- "not void"
  deriving (Eq, Ord)

instance Show RetType where
  show TyVoid = "void"
  show (NV t) = show t

instance Show Type where
  show TyBool = "bool"
  show TyInt  = "int"
