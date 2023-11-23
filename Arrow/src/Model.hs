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
newtype Ident = Ident String deriving Show
data Dir      = Left | Right | Front deriving Show
data Pattern  = Empty | Lambda | Debris | Asteroid | Boundary deriving Show
data Alt      = UnderScore | Alt Pattern Commands deriving Show
data Command  = Go
              | Take
              | Mark
              | Nothing
              | Turn Dir
              | Case Dir Alts Commands deriving Show

newtype Alts     = Alts [Alt] deriving Show
newtype Commands = Commands [Command] deriving Show
data Rule        = Rule Ident Commands deriving Show

newtype Program  = Program [Rule] deriving Show
