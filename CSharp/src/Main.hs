module Main where

import           CSharp.AbstractSyntax
import           CSharp.Algebra
import           CSharp.CodeGen
import           CSharp.Parser

import           SSM

import           ParseLib.Abstract.Derived
import           ParseLib.Error            (ErrorsPretty)

import           Prelude                   hiding ((*>), (<$), (<*))
import           System.Environment
import           System.FilePath
import           CSharp.Analysis.ScopeAnalysis
import           Data.Either.Validation
import           Data.List
import CSharp.Analysis.TypeAnalysis (typeAnalysisAlgebra)

main :: IO ()
main = do
  -- get command line arguments
  args <- getArgs
  files <- case args of
    [] -> do
      putStrLn "no argument given; assuming example.cs"
      return ["example.cs"]
    xs -> return xs
  -- translate each of the files
  mapM_ processFile files

-- processFile compiles one file;
-- it take the name of the input file
processFile :: FilePath -> IO ()
processFile infile = do
  let outfile = addExtension (dropExtension infile) "ssm"
  xs <- readFile infile
  let program = run "parser" (pClass <* eof) . run "lexer" lexicalScanner $ xs

  case foldCSharp scopeAnalysisAlgebra program of
    Failure err -> putStrLn $ intercalate "\n\n" err
    Success _   -> case foldCSharp typeAnalysisAlgebra program of
      Failure err -> putStrLn $ intercalate "\n\n" err
      Success _   -> do
        let ssm = formatCode $ foldCSharp codeAlgebra program
        writeFile outfile ssm
        putStrLn (outfile ++ " written")

run :: (ErrorsPretty s, Ord s, Show a) => String -> Parser s a -> [s] -> a
run s p x = fst . headOrError . parse (p <* eof) $ x
    where
  headOrError (x : xs) = x
  headOrError []       = error $ "The " <> s <> " returned no full parses."
