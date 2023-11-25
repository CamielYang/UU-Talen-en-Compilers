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
  case 	        {TCase}
  of		{TOf}
  end 	        {TEnd}
  '->' 		{TArrow}
  '_' 		{TUnderScore}
  ',' 		{TComma}
  '.' 		{TDot}
  ';' 		{TSemiColon}



%%

Program : Rules                      { Program $1 }

Rules   : Rule                       { [$1] }
        | Rules Rule                 { $2 : $1 }
Rule    : ident '->' Cmds '.'        { Rule $1 $3 }

Cmds    : Cmds1                      { Cmds $1 }
Cmds1   : {- empty -}                { [] }
        | Cmd                        { [$1] }
        | Cmds1 ',' Cmd              { $3 : $1 }
Cmd     : go                         { CMDGo }
        | take                       { CMDTake }
        | mark                       { CMDMark }
        | nothing                    { CMDNothing }
        | turn Dir                   { CMDTurn $2 }
        | case Dir of Alts end       { CMDCase $2 $4 }
        | ident                      { CMDIdent $1 }

Dir     : left                       { DLeft }
        | right                      { DRight }
        | front                      { DFront }


Alts    : Alts1                      { Alts $1 }
Alts1   : {- empty -}                { [] }
        | Alt                        { [$1] }
        | Alts1 ';' Alt              { $3 : $1 }
Alt     : Pattern '->' Cmds          { Alt $1 $3 }

Pattern : Empty                      { PEmpty }
        | Lambda                     { PLambda }
        | Debris                     { PDebris }
        | Asteroid                   { PAsteroid }
        | Boundary                   { PBoundary }
        | '_'                        { PUnderScore }

{

happyError _ = error "parse error"

}