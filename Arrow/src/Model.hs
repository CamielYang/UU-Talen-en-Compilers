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
type IdentT       = String

data Pattern     = PEmpty
                 | PLambda
                 | PDebris
                 | PAsteroid
                 | PBoundary
                 | PUnderScore       deriving (Show, Eq)

data Dir         = DLeft
                 | DRight
                 | DFront            deriving (Show, Eq)

data Cmd         = CMDGo
                 | CMDTake
                 | CMDMark
                 | CMDNothing
                 | CMDTurn  Dir
                 | CMDCase  Dir Alts
                 | CMDIdent IdentT   deriving Show

newtype Cmds     = Cmds [Cmd]        deriving Show

data Alt         = Alt Pattern Cmds  deriving Show
newtype Alts     = Alts [Alt]        deriving Show

data Rule        = Rule IdentT Cmds  deriving Show

newtype Program  = Program [Rule]    deriving Show
