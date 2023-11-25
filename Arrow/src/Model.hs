module Model where
import           Data.Sequence (Seq (Empty))

-- Exercise 1
data Token = Token
           -- Commands
           | TArrow
           | TDot
           | TComma
           | TGo
           | TTake
           | TMark
           | TNothing
           | TTurn
           | TCase
           | TOf
           | TEnd
           -- Directions
           | TLeft
           | TRight
           | TFront
           | TSemiColon
           --  Pattern
           | TEmpty
           | TLambda
           | TDebris
           | TAsteroid
           | TBoundary
           | TUnderScore
           --  Ident
           | TIdent String
  deriving Show


-- Exercise 2
data Pattern  = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PUnderScore deriving Show

data Dir      = DLeft | DRight | DFront deriving Show

data Cmd     = CMDGo
             | CMDTake
             | CMDMark
             | CMDNothing
             | CMDTurn Dir
             | CMDCase Dir Alts
             | CMDIdent String deriving Show

newtype Cmds = Cmds [Cmd] deriving Show

data Alt      = Alt Pattern Cmds deriving Show
newtype Alts  = Alts [Alt] deriving Show

data Rule     = Rule String Cmds deriving Show

newtype Program  = Program [Rule] deriving Show
