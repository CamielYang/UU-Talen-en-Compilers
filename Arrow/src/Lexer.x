{
module Lexer where

import Model
}

%wrapper "basic"

$digit = [0-9]    -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  -- Whitespace and comments
  "--".*                 ;
  $white+                ;

  -- Commands
  "go"                   { \s -> TGo }
  "take"                 { \s -> TTake }
  "mark"                 { \s -> TMark }
  "nothing"              { \s -> TNothing }
  "turn"                 { \s -> TTurn }

  -- Directions
  "left"                 { \s -> TLeft }
  "right"                { \s -> TRight }
  "front"                { \s -> TFront }

  --  Pattern
  "Empty"                { \s -> TEmpty }
  "Lambda"               { \s -> TLambda }
  "Debris"               { \s -> TDebris }
  "Asteroid"             { \s -> TAsteroid }
  "Boundary"             { \s -> TBoundary }
  "_"                    { \s -> TUnderScore }

  -- Case Keywords
  "case"                 { \s -> TCase }
  "of"                   { \s -> TOf }
  "end"                  { \s -> TEnd }

  -- Symbols
  ";"                    { \s -> TSemiColon }
  "->"                   { \s -> TArrow }
  "."                    { \s -> TDot }
  ","                    { \s -> TComma }

  --  Ident
  [$alpha $digit \+ \-]+ { \s -> TIdent s }



