module Main where

import           Algebra
import           Interpreter
import           Lexer
import           Model
import           Parser

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  putStrLn "Press enter to execute a step:"
  getLine
  case step env state of
    Done space _ _ -> do
      putStrLn $ printSpace space
    Ok state'@(ArrowState space _ _ _) -> do
      putStrLn $ printSpace space
      interactive env state'
    Fail err -> putStrLn err


batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
  chars <- readFile "examples/Add.arrow"
  putStrLn "Input program:"
  putStrLn ""
  putStrLn chars
  putStrLn ""
  let tokens = alexScanTokens chars
  putStrLn "Tokens:"
  putStrLn ""
  print tokens
  let arr = parser tokens
  putStrLn "Parsed program:"
  putStrLn ""
  print arr

