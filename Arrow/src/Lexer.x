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
  "go"                   { const TGo      }
  "take"                 { const TTake    }
  "mark"                 { const TMark    }
  "nothing"              { const TNothing }
  "turn"                 { const TTurn    }

  -- Directions
  "left"                 { const TLeft  }
  "right"                { const TRight }
  "front"                { const TFront }

  --  Pattern
  "Empty"                { const TEmpty      }
  "Lambda"               { const TLambda     }
  "Debris"               { const TDebris     }
  "Asteroid"             { const TAsteroid   }
  "Boundary"             { const TBoundary   }
  "_"                    { const TUnderScore }

  -- Case Keywords
  "case"                 { const TCase }
  "of"                   { const TOf   }
  "end"                  { const TEnd  }

  -- Symbols
  ";"                    { const TSemiColon }
  "->"                   { const TArrow     }
  "."                    { const TDot       }
  ","                    { const TComma     }

  --  Ident
  [$alpha $digit \+ \-]+ { TIdent }



