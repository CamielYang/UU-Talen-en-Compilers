module CSharp.Analysis.Error where

createError :: String -> [String] -> [String]
createError title content = [title <> ": \n  " <> unwords content]

scopeError :: [String] -> [String]
scopeError = createError "ScopeError"

typeError :: [String] -> [String]
typeError = createError "TypeError"