module Algebra where

import           Data.List
import           GHC.IO    (noDuplicate)
import           Model


-- Exercise 5
type CmdAlgebra r = (r,                -- CMDGo
                     r,                -- CMDTake
                     r,                -- CMDMark
                     r,                -- CMDNothing
                     Dir -> r,         -- CMDTurn Dir
                     Dir -> Alts -> r, -- CMDCase Dir Alts
                     String -> r)      -- CMDIdent String
foldCmd :: CmdAlgebra r -> Cmd -> r
foldCmd (go,
         take,
         mark,
         nothing,
         turn,
         case',
         ident) = f
  where
    f CMDGo              = go
    f CMDTake            = take
    f CMDMark            = mark
    f CMDNothing         = nothing
    f (CMDTurn dir)      = turn dir
    f (CMDCase dir alts) = case' dir alts
    f (CMDIdent id)      = ident id

-- Exercise 6

type ProgramAlgebra r = ([Rule] -> r) -- Program
foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram f (Program rules) = f rules

noUndefinedAlg :: ProgramAlgebra Bool
noUndefinedAlg = all f
  where
    f (Rule _ _) = True
    f undefined  = False

noUndefined :: Program -> Bool
noUndefined = foldProgram noUndefinedAlg

hasStartAlg :: ProgramAlgebra Bool
hasStartAlg = any (\(Rule id _) -> id == "start")

hasStart :: Program -> Bool
hasStart = foldProgram hasStartAlg

noDuplicatesAlg :: ProgramAlgebra Bool
noDuplicatesAlg = f []
  where
    f :: [String] -> [Rule] -> Bool
    f _ []                    = True
    f ids (Rule id _ : rules) = id `notElem` ids && f (id : ids) rules

noDuplicates :: Program -> Bool
noDuplicates = foldProgram noDuplicatesAlg

validCmdCaseAlg :: CmdAlgebra Bool
validCmdCaseAlg = (True, True, True, True, isTrue, case', isTrue)
  where
    isTrue _ = True

    case' :: Dir -> Alts -> Bool
    case' _ (Alts alts) = hasUnderScore || hasAllPatterns
      where
        hasUnderScore = case find f alts of
                          Nothing -> False
                          Just _  -> True
          where
            f (Alt PUnderScore _) = True
            f _                   = False
        hasAllPatterns = length ps == 5
          where
            ps = nub $ foldr f [] alts
            f (Alt PUnderScore _) b = b
            f (Alt p _) b           = p : b

validCmdCase :: Cmd -> Bool
validCmdCase = foldCmd validCmdCaseAlg

validCmdCaseProgAlg :: ProgramAlgebra Bool
validCmdCaseProgAlg = f
  where
    f :: [Rule] -> Bool
    f []                    = True
    f (Rule _ cmds : rules) = validCmdCaseCmds cmds && f rules

    validCmdCaseCmds :: Cmds -> Bool
    validCmdCaseCmds (Cmds cmds) = all validCmdCase cmds

validCmdCaseProg :: Program -> Bool
validCmdCaseProg = foldProgram validCmdCaseProgAlg

testProgram :: Program
testProgram = Program [Rule "start" (Cmds [CMDTurn DRight,CMDGo,CMDTurn DLeft,CMDIdent "firstArg"]),Rule "turnAround" (Cmds [CMDTurn DRight,CMDTurn DRight]),Rule "return" (Cmds [CMDCase DFront (Alts [Alt PBoundary (Cmds [CMDNothing]),Alt PUnderScore (Cmds [CMDGo,CMDIdent "return"])])]),Rule "firstArg" (Cmds [CMDCase DLeft (Alts [Alt PLambda (Cmds [CMDGo,CMDIdent "firstArg",CMDMark,CMDGo]),Alt PUnderScore (Cmds [CMDIdent "turnAround",CMDIdent "return",CMDTurn DLeft,CMDGo,CMDGo,CMDTurn DLeft,CMDIdent "secondArg"])])]),Rule "secondArg" (Cmds [CMDCase DLeft (Alts [Alt PLambda (Cmds [CMDGo,CMDIdent "secondArg",CMDMark,CMDGo]),Alt PUnderScore (Cmds [CMDIdent "turnAround",CMDIdent "return",CMDTurn DLeft,CMDGo,CMDTurn DLeft])])])]

checkProgram :: Program -> Bool
checkProgram p = noUndefined      p
              && hasStart         p
              && noDuplicates     p
              && validCmdCaseProg p

