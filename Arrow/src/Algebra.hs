module Algebra where

import           Data.List
import           GHC.IO    (noDuplicate)
import           Model


-- Exercise 5
data CmdAlgebra r = CmdAlg {
  cmdGo      :: r,
  cmdTake    :: r,
  cmdMark    :: r,
  cmdNothing :: r,
  cmdTurn    :: Dir -> r,
  cmdCase    :: Dir -> Alts -> r,
  cmdIdent   :: String -> r
}

baseCmdAlgebra :: CmdAlgebra Bool
baseCmdAlgebra = CmdAlg {
  cmdGo      = True,
  cmdTake    = True,
  cmdMark    = True,
  cmdNothing = True,
  cmdTurn    = const True,
  cmdCase    = \_ _ -> True,
  cmdIdent   = const True
}

foldCmd :: CmdAlgebra r -> Cmd -> r
foldCmd alg = f
  where
    f CMDGo              = cmdGo      alg
    f CMDTake            = cmdTake    alg
    f CMDMark            = cmdMark    alg
    f CMDNothing         = cmdNothing alg
    f (CMDTurn dir)      = cmdTurn    alg dir
    f (CMDCase dir alts) = cmdCase    alg dir alts
    f (CMDIdent id)      = cmdIdent   alg id

-- Exercise 6

type ProgramAlgebra r = [Rule] -> r -- Program
foldProgram :: ProgramAlgebra r -> Program -> r
foldProgram f (Program rules) = f rules

validCmdAlg :: [Rule] -> CmdAlgebra Bool
validCmdAlg rs = baseCmdAlgebra { cmdIdent = ident }
  where
    ident id = any (\(Rule id' _) -> id == id') rs
validCmd :: [Rule] -> Cmd -> Bool
validCmd rs = foldCmd $ validCmdAlg rs

noUndefinedRulesAlg :: ProgramAlgebra Bool
noUndefinedRulesAlg rs = all f rs
  where
    f (Rule id (Cmds cmds)) = all (validCmd rs) cmds
noUndefinedRules :: Program -> Bool
noUndefinedRules = foldProgram noUndefinedRulesAlg

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
validCmdCaseAlg = baseCmdAlgebra { cmdCase = case' }
  where
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
checkProgram p = noUndefinedRules p
              && hasStart         p
              && noDuplicates     p
              && validCmdCaseProg p

