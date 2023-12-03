module Interpreter where

import           ParseLib.Abstract
import           Prelude           hiding ((<$), (<*))

import           Data.Map          (Map)
import qualified Data.Map          as L

import           Control.Monad     (replicateM)
import           Data.Char         (isSpace)

import           Algebra
import           Data.Foldable     (find)
import           Data.Maybe        (Maybe, fromJust, fromMaybe)
import           Debug.Trace
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
data Compass = North | East | South | West deriving (Show, Eq, Enum, Ord, Bounded)
test = [West .. West]

cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x
  | x == minBound = maxBound
  | otherwise     = pred x

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x
  | x == maxBound = minBound
  | otherwise     = succ x

type Ident = IdentT
type Commands = Cmds
type Heading = Compass

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
updatePos :: Pos -> Heading -> Pos
updatePos (y, x) North = (y - 1, x    )
updatePos (y, x) East  = (y    , x + 1)
updatePos (y, x) South = (y + 1, x    )
updatePos (y, x) West  = (y    , x - 1)

takePattern :: Space -> Pos -> Space
takePattern spaceMap pos
  | pattern' `elem` [Lambda, Debris] = L.insert pos Empty spaceMap
  | otherwise                          = spaceMap
  where
    pattern' = fromJust $ L.lookup pos spaceMap

markPattern :: Space -> Pos -> Space
markPattern spaceMap pos = L.insert pos Lambda spaceMap

makeTurn :: Heading -> Dir -> Heading
makeTurn h dir
  | dir == DLeft  = cyclePrev h
  | dir == DRight = cycleNext h
  | otherwise     = h

doCase :: Dir -> Alts -> ArrowState -> ArrowState
doCase dir (Alts alts) (ArrowState sp p h (Cmds cds)) = trace (show alts ++ show pattern') $
  case find (\(Alt p _) -> p == pattern') alts of
    Nothing               -> let (Alt _ (Cmds c)) = fromJust $ find (\(Alt p _) -> p == PUnderScore) alts in ArrowState sp p h (Cmds $ c ++ cds)
    Just (Alt _ (Cmds c)) -> ArrowState sp p h (Cmds $ c ++ cds)
  where
    readPos = case dir of
      DFront -> updatePos p h
      DLeft  -> updatePos p (cyclePrev h)
      DRight -> updatePos p (cycleNext h)
    pattern' = case fromMaybe Boundary $ L.lookup readPos sp of
      Empty    -> PEmpty
      Lambda   -> PLambda
      Debris   -> PDebris
      Asteroid -> PAsteroid
      Boundary -> PBoundary

step :: Environment -> ArrowState -> Step
step env as@(ArrowState sp p h (Cmds [])) = Done sp p h
step env as@(ArrowState sp p h st@(Cmds (cd : cds))) =
  trace (show p ++ show st ++ show h ++ "\n") $ case cd of
    CMDGo            -> Ok $ ArrowState sp (updatePos p h) h (Cmds cds)
    CMDTake          -> Ok $ ArrowState (takePattern sp p) p h (Cmds cds)
    CMDMark          -> Ok $ ArrowState (markPattern sp p) p h (Cmds cds)
    CMDNothing       -> Ok $ ArrowState sp p h (Cmds cds)
    CMDTurn dir      -> Ok $ ArrowState sp p (makeTurn h dir) (Cmds cds)
    CMDCase dir alts -> Ok $ doCase dir alts (ArrowState sp p h (Cmds cds))
    CMDIdent ident   ->
      case L.lookup ident env of
        Nothing          -> Fail "Identifier not found"
        Just (Cmds cds') -> Ok $ ArrowState sp p h (Cmds $ cds' ++ cds)


