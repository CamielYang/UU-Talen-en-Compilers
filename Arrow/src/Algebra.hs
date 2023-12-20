{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Algebra where

import           Data.List
import           Debug.Trace
import           GHC.IO      (noDuplicate)
import           Model


-- Exercise 5
data Algebra p d c a r pr = Algebra {
  -- Pattern
  pEmpty      :: p,
  pLambda     :: p,
  pDebris     :: p,
  pAsteroid   :: p,
  pBoundary   :: p,
  pUnderScore :: p,

  -- Directions
  dLeft       :: d,
  dRight      :: d,
  dFront      :: d,

  -- Commands
  go          :: c,
  take'       :: c,
  mark        :: c,
  nothing     :: c,
  turn        :: d -> c,
  case'       :: d -> [a] -> c,
  ident       :: IdentT -> c,

  -- Alternatives
  alt         :: p -> [c] -> a,

  -- Rules
  rule        :: IdentT -> [c] -> r,

  -- Program
  program     :: [r] -> pr
}

foldAlgebra :: Algebra p d c a r pr -> Program -> pr
foldAlgebra alg = foldProgram
  where
    foldDir DLeft  = dLeft alg
    foldDir DRight = dRight alg
    foldDir DFront = dFront alg

    foldPattern PEmpty          = pEmpty alg
    foldAlt (Alt p (Cmds cmds)) = alt alg (foldPattern p) (map foldCmd cmds)

    foldCmd CMDGo                     = go alg
    foldCmd CMDTake                   = take' alg
    foldCmd CMDMark                   = mark alg
    foldCmd CMDNothing                = nothing alg
    foldCmd (CMDTurn dir)             = turn alg (foldDir dir)
    foldCmd (CMDCase dir (Alts alts)) = case' alg (foldDir dir) (map foldAlt alts)
    foldCmd (CMDIdent id)             = ident alg id

    foldRule (Rule id (Cmds cmds)) = rule alg id (map foldCmd cmds)
    foldProgram (Program rules)    = program alg (map foldRule rules)


-- Exercise 6
baseAlgebra = Algebra {
  -- Pattern
  pEmpty      = True,
  pLambda     = True,
  pDebris     = True,
  pAsteroid   = True,
  pBoundary   = True,
  pUnderScore = True,

  -- Directions
  dLeft       = True,
  dRight      = True,
  dFront      = True,

  -- Commands
  go          = True,
  take'       = True,
  mark        = True,
  nothing     = True,
  turn        = const True,
  case'       = \_ _ -> True,
  ident       = const True,

  -- Alternatives
  alt         = \_ _ -> True,

  -- Rules
  rule        = \_ _ -> True,

  -- Program
  program     = const True
}

hasStart :: Program -> Bool
hasStart = foldAlgebra baseAlgebra {
    rule    = const,
    program = elem "start"
  }

noDuplicates :: Program -> Bool
noDuplicates = foldAlgebra baseAlgebra {
    rule   = const,
    program = \rules -> nub rules == rules
  }

validCmdCase :: Program -> Bool
validCmdCase = foldAlgebra baseAlgebra {
    pEmpty      = PEmpty,
    pLambda     = PLambda,
    pDebris     = PDebris,
    pAsteroid   = PAsteroid,
    pBoundary   = PBoundary,
    pUnderScore = PUnderScore,
    alt         = const,
    case'       = \_ alts -> PUnderScore `elem` alts
               || length (nub alts) == 5
  }

noUndefinedRules :: Program -> Bool
noUndefinedRules = foldAlgebra baseAlgebra {
    go      = []
  , take'   = []
  , mark    = []
  , nothing = []
  , turn    = const []
  , case'   = \_ alts -> concat alts
  , ident   = (: [])

  , alt     = \ _ cmds -> concat cmds
  , rule    = (,)

  , program = \rs -> let (ids, cmds) = unzip rs in
      all (all (all (`elem` ids))) cmds
  }

testProgram :: Program
testProgram = Program [Rule "start" (Cmds [CMDTurn DRight,CMDGo,CMDTurn DLeft,CMDIdent "firstArg"]),Rule "turnAround" (Cmds [CMDTurn DRight,CMDTurn DRight]),Rule "return" (Cmds [CMDCase DFront (Alts [Alt PBoundary (Cmds [CMDNothing]),Alt PUnderScore (Cmds [CMDGo,CMDIdent "return"])])]),Rule "firstArg" (Cmds [CMDCase DLeft (Alts [Alt PLambda (Cmds [CMDGo,CMDIdent "firstArg",CMDMark,CMDGo]),Alt PUnderScore (Cmds [CMDIdent "turnAround",CMDIdent "return",CMDTurn DLeft,CMDGo,CMDGo,CMDTurn DLeft,CMDIdent "secondArg"])])]),Rule "secondArg" (Cmds [CMDCase DLeft (Alts [Alt PLambda (Cmds [CMDGo,CMDIdent "secondArg",CMDMark,CMDGo]),Alt PUnderScore (Cmds [CMDIdent "turnAround",CMDIdent "return",CMDTurn DLeft,CMDGo,CMDTurn DLeft])])])]

checkProgram :: Program -> Bool
checkProgram p = noUndefinedRules p
              && hasStart        p
              && noDuplicates    p
              && validCmdCase    p
