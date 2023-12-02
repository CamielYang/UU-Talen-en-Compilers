module Interpreter where

import           ParseLib.Abstract
import           Prelude           hiding ((<$), (<*))

import           Data.Map          (Map)
import qualified Data.Map          as L

import           Control.Monad     (replicateM)
import           Data.Char         (isSpace)

import           Algebra
import           Data.Maybe        (fromJust)
import           Lexer
import           Model
import           Parser


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Show)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces

-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]

-- Exercise 7
printSpace :: Space -> String
printSpace spaceMap =
    show k ++ "\n"
 ++ space
  where
    (k@(rws, clms),_) = L.findMax spaceMap
    space = L.foldrWithKey f [] spaceMap
    f (_, clms') content b
      | clms' == clms = cChar : '\n' : b
      | otherwise     = cChar : b
      where
        cChar = fromJust $ lookup content contentsTable

testPrintSampleSpace = do
  s <- readFile "examples/SampleSpace.space"
  let [(result, _)] = parse parseSpace s
  putStrLn $ printSpace result

-- These three should be defined by you
type Ident = IdentT
type Commands = Cmds
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment chars
  | checkProgram pgrm = L.fromList $ map (\(Rule i c) -> (i, c)) rs
  | otherwise         = error "Program is not valid"
  where
    tokens :: [Token]
    tokens = alexScanTokens chars
    pgrm :: Program
    pgrm@(Program rs) = parser tokens

testEnvironment = do
  s <- readFile "examples/Add.arrow"
  return $ toEnvironment s

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


