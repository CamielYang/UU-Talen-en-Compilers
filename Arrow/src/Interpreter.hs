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


data Contents  = Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Show)

type Size      = Int
type Pos       = (Int, Int)
type Space     = Map Pos Contents

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

type Ident       = IdentT
type Commands    = Cmds
type Heading     = Compass

type Environment = Map Ident Commands

type Stack       = Commands
data ArrowState  = ArrowState Space Pos Heading Stack

data Step        = Done Space Pos Heading
                 | Ok   ArrowState
                 | Fail String

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
pop :: Stack -> Stack
pop (Cmds [])     = Cmds []
pop (Cmds (_:xs)) = Cmds xs

prepend :: Cmds -> Stack -> Stack
prepend (Cmds cs) (Cmds cs') = Cmds (cs ++ cs')

contentToPattern :: Contents -> Pattern
contentToPattern Empty    = PEmpty
contentToPattern Lambda   = PLambda
contentToPattern Debris   = PDebris
contentToPattern Asteroid = PAsteroid
contentToPattern Boundary = PBoundary

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

sensoryRead :: ArrowState -> Dir -> Pattern
sensoryRead (ArrowState sp p h st) dir = contentToPattern $ fromMaybe Boundary $ L.lookup readPos sp
  where
    readPos =
      case dir of
        DFront -> updatePos p h
        DLeft  -> updatePos p (cyclePrev h)
        DRight -> updatePos p (cycleNext h)

goCommand :: ArrowState -> Step
goCommand (ArrowState sp p h st)       = Ok $ ArrowState sp (updatePos p h) h (pop st)

takeCommand :: ArrowState -> Step
takeCommand (ArrowState sp p h st)     = Ok $ ArrowState (takePattern sp p) p h (pop st)

markCommand :: ArrowState -> Step
markCommand (ArrowState sp p h st)     = Ok $ ArrowState (markPattern sp p) p h (pop st)

nothingCommand :: ArrowState -> Step
nothingCommand (ArrowState sp p h st)  = Ok $ ArrowState sp p h (pop st)

turnCommand :: ArrowState -> Dir -> Step
turnCommand (ArrowState sp p h st) dir = Ok $ ArrowState sp p (makeTurn h dir) (pop st)

caseCommand :: ArrowState -> Dir -> Alts -> Step
caseCommand (ArrowState sp p h st) dir (Alts alts) = Ok $
  case find (\(Alt p _) -> p == scannedPattern) alts of
    Nothing ->
      let (Alt _ caseCmds) = fromJust $ find (\(Alt p _) -> p == PUnderScore) alts in
      ArrowState sp p h (prepend caseCmds (pop st))
    Just (Alt _ caseCmds) -> ArrowState sp p h (prepend caseCmds (pop st))
  where
    scannedPattern = sensoryRead (ArrowState sp p h st) dir

identCommand :: ArrowState -> Environment -> Ident -> Step
identCommand (ArrowState sp p h st) env ident =
  case L.lookup ident env of
    Nothing     -> Fail "Identifier not found"
    Just idCmds -> Ok $ ArrowState sp p h (prepend idCmds (pop st))

step :: Environment -> ArrowState -> Step
step env as@(ArrowState sp p h (Cmds [])) = Done sp p h
step env as@(ArrowState sp p h st@(Cmds (cd : cds))) =
  case cd of
    CMDGo            -> goCommand      as
    CMDTake          -> takeCommand    as
    CMDMark          -> markCommand    as
    CMDNothing       -> nothingCommand as
    CMDTurn dir      -> turnCommand    as dir
    CMDCase dir alts -> caseCommand    as dir alts
    CMDIdent ident   -> identCommand   as env ident


