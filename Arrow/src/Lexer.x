{
module Lexer where

import Model
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  "--".*                 ;
  $white+                ;

  "->"                   { \s -> TArrow }
  "."                    { \s -> TDot }
  ","                    { \s -> TComma }
  "go"                   { \s -> TGo }
  "take"                 { \s -> TTake }
  "mark"                 { \s -> TMark }
  "nothing"              { \s -> TNothing }
  "turn"                 { \s -> TTurn }
  "case"                 { \s -> TCase }
  "of"                   { \s -> TOf }
  "end"                  { \s -> TEnd }

  "left"                 { \s -> TLeft }
  "right"                { \s -> TRight }
  "front"                { \s -> TFront }
  ";"                    { \s -> TSemiColon }

  "Empty"                { \s -> TEmpty }
  "Lambda"               { \s -> TLambda }
  "Debris"               { \s -> TDebris }
  "Asteroid"             { \s -> TAsteroid }
  "Boundary"             { \s -> TBoundary }
  "_"           { \s -> TUnderScore }

  [$alpha $digit \+ \-]+ { \s -> TIdent s }



