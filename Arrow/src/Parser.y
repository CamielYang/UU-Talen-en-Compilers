{
module Parser where

import Model
import qualified Lexer as L
}

%name parser
%tokentype { Token }

%token
  Empty  	{TEmpty}
  Lambda 	{TLambda}
  Debris 	{TDebris}
  Asteroid 	{TAsteroid}
  Boundary 	{TBoundary}
  left 		{TLeft}
  right 	{TRight}
  front 	{TFront}
  ident 	{TIdent $$}
  go 		{TGo}
  take 	        {TTake}
  mark 	        {TMark}
  nothing       {TNothing}
  turn 	        {TTurn}
  "case" 	{TCase}
  "of" 		{TOf}
  "end" 	{TEnd}
  "->" 		{TArrow}
  "_" 		{TUnderScore}
  "," 		{TComma}
  "." 		{TDot}
  ";" 		{TSemiColon}



%%

Pattern : Empty                      { PEmpty }
        | Lambda                     { PLambda }
        | Debris                     { PDebris }
        | Asteroid                   { PAsteroid }
        | Boundary                   { PBoundary }
        | "_"                        { PUnderScore }

Dir     : left                       { DLeft }
        | right                      { DRight }
        | front                      { DFront }

Cmd     : go                         { CMDGo }
        | take                       { CMDTake }
        | mark                       { CMDMark }
        | nothing                    { CMDNothing }
        | turn Dir                   { CMDTurn $2 }
        | "case" Dir "of" Alts "end" { CMDCase $2 $4 }
        | ident                      { CMDIdent $1 }

Cmds    : Cmds1 ","                  { Cmds $1 }
Cmds1   : {- empty -}                { [] }
        | Cmds1 Cmd                  { $2 : $1 }

Alt     : Pattern "->" Cmds          { Alt $1 $3 }

Alts    : Alts1 ";"                  { Alts $1 }
Alts1   : {- empty -}                { [] }
        | Alts1 Alt                  { $2 : $1 }

Rule    : ident "->" Cmds "."        { Rule $1 $3 }
Rule1   : Rule                       { [$1] }
        | Rule1 Rule                 {$2 : $1}

Program : Rule1                      { Program $1 }

{

happyError _ = error "parse error"

}