module Main where

import           CSharp.AbstractSyntax
import           CSharp.Algebra
import           CSharp.Analysis
import           CSharp.CodeGen
import           CSharp.Parser

import           SSM

import           ParseLib.Abstract.Derived
import           ParseLib.Error            (ErrorsPretty)

import           CSharp.CodeGen            (codeAlgebra)
import           CSharp.Parser             (lexicalScanner)
import           Prelude                   hiding ((*>), (<$), (<*))
import           System.Environment
import           System.FilePath
import CSharp.Analysis.ScopeAnalysis
import Data.Either

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
  -- let program = run "lexer" lexicalScanner $ xs
  -- putStrLn (show program)
  let test = foldCSharp scopeAnalysisAlgebra program
  putStrLn (show $ test)

  case foldCSharp analysisAlgebra program of
    False -> error "analysis failed"
    True -> do
      let ssm = formatCode $ foldCSharp codeAlgebra program
      writeFile outfile ssm
      putStrLn (outfile ++ " written")

run :: (ErrorsPretty s, Ord s, Show a) => String -> Parser s a -> [s] -> a
run s p x = fst . headOrError . parse (p <* eof) $ x
    where
  headOrError (x : xs) = x
  headOrError []       = error $ "The " <> s <> " returned no full parses."

testExpr =
  -- run "parser" (pClass <* eof) .
  -- run "lexer" lexicalScanner
  -- $ "class Hello {\nvoid main() {\nint b;\nb = 1;\n}\n}"
  run "parser" (pStat <* eof) .
  run "lexer" lexicalScanner
  $ "for (int i; i < 10;) {}"
