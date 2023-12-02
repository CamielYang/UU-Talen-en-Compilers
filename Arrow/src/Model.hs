module Model where
import           Data.Sequence (Seq (Empty))

-- Exercise 1
data Token = Token
           -- Commands
           | TGo
           | TTake
           | TMark
           | TNothing
           | TTurn
           | TIdent String

           -- Case Keywords
           | TCase
           | TOf
           | TEnd

           -- Directions
           | TLeft
           | TRight
           | TFront

           --  Pattern
           | TEmpty
           | TLambda
           | TDebris
           | TAsteroid
           | TBoundary
           | TUnderScore

           -- Symbols
           | TSemiColon
           | TArrow
           | TDot
           | TComma
  deriving Show


-- Exercise 2
newtype Ident    = Ident String       deriving Show

data Pattern     = PEmpty
                 | PLambda
                 | PDebris
                 | PAsteroid
                 | PBoundary
                 | PUnderScore        deriving (Show, Eq)

data Dir         = DLeft
                 | DRight
                 | DFront             deriving Show

data Cmd         = CMDGo
                 | CMDTake
                 | CMDMark
                 | CMDNothing
                 | CMDTurn  Dir
                 | CMDCase  Dir Alts
                 | CMDIdent Ident    deriving Show

newtype Cmds     = Cmds [Cmd]        deriving Show

data Alt         = Alt Pattern Cmds  deriving Show
newtype Alts     = Alts [Alt]        deriving Show

data Rule        = Rule Ident Cmds   deriving Show

newtype Program  = Program [Rule]    deriving Show
