module Main where

import           Algebra
import qualified Data.Map          as L
import           Data.Maybe        (fromJust)
import           Interpreter
import           Lexer
import           Model
import           ParseLib.Abstract
import           Parser

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  putStrLn "Press enter to execute a step:"
  getLine
  case step env state of
    Done space _ _ -> do
      putStrLn $ "Space result:" ++ "\n"
              ++ printSpace space
    Ok state'@(ArrowState space pos heading stack) -> do
      putStrLn $ "Position: " ++ show pos ++ "\n"
              ++ "Heading: " ++ show heading ++ "\n"
              ++ "Stack: " ++ show stack ++ "\n"
              ++ printSpace space
      interactive env state'
    Fail err -> putStrLn err

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state = case step env state of
    Done s p h                         -> (s, p, h)
    Ok state'@(ArrowState space _ _ _) -> batch env state'
    Fail err                           -> error err

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
-- main :: IO ()
-- main = do
--   chars <- readFile "examples/Add.arrow"
--   putStrLn "Input program:"
--   putStrLn ""
--   putStrLn chars
--   putStrLn ""
--   let tokens = alexScanTokens chars
--   putStrLn "Tokens:"
--   putStrLn ""
--   print tokens
--   let arr = parser tokens
--   putStrLn "Parsed program:"
--   putStrLn ""
--   print arr

-- Adding natural numbers
arrowFile      = "examples/Add.arrow"
spaceFile      = "examples/AddInput.space"
initialPos     = (0, 0)
initialHeading = East

-- Remove debris
-- arrowFile      = "examples/RemoveDebris.arrow"
-- spaceFile      = "examples/SampleSpace.space"
-- initialPos     = (1, 4)
-- initialHeading = North

-- Maze
-- arrowFile      = "examples/Find.arrow"
-- spaceFile      = "examples/Maze.space"
-- initialPos     = (0,2) -- (0, 0), (7, 7), (5, 0) (0, 2)
-- initialHeading = North

main :: IO ()
main = do
  arrowChars <- readFile arrowFile
  spaceChars <- readFile spaceFile

  let env = toEnvironment arrowChars
  let startCmds = L.findWithDefault (Cmds []) "start" env
  let [(space, _)] = parse parseSpace spaceChars
  let state = ArrowState space initialPos initialHeading startCmds

  askForInput env state

askForInput :: Environment -> ArrowState -> IO ()
askForInput env state = do
  putStrLn $ "ArrowFile      = " ++ arrowFile           ++ "\n"
          ++ "SpaceFile      = " ++ spaceFile           ++ "\n"
          ++ "InitialPos     = " ++ show initialPos     ++ "\n"
          ++ "InitialHeading = " ++ show initialHeading ++ "\n\n"
          ++ "1 - Interactive mode"                     ++ "\n"
          ++ "2 - Batch mode"                           ++ "\n"
          ++ "3 - Exit"                                 ++ "\n"
  choice <- getLine
  case choice of
    "1" -> interactive env state
    "2" -> do
      let (ArrowState spaceStart _ _ _) = state
      let (spaceResult, pos', heading') = batch env state

      putStrLn $ "Space start:" ++ "\n"
              ++ printSpace spaceStart
      putStrLn $ "Space result:" ++ "\n"
              ++ printSpace spaceResult
    "3" -> return ()
    _   -> do
      putStrLn "Invalid input"
      askForInput env state


