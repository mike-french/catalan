%% coding: latin-1
%--------------------------------------------------------------------
%
% Copyright (c) 2015 Mike French
%
% This software is released under the MIT license
% http://www.opensource.org/licenses/mit-license.php
%
%--------------------------------------------------------------------

-module( brackets ).

-export( [example/0, run/1] ).

-import( lists, [sublist/3, nth/2, seq/3, flatten/1] ).

% -----------------------------------------------------
%
% Problem
% -------
%
% Given a simple flat arithmetic expression, generate
% all the possible bracketings of terms for evaluation.
% 
% Input
% -----
% 
% A string of interleaved single-digit non-zero integers
% and binary arithmetic operators as follows:
%
%  add:       '+'
%  subract:   '-'
%  multiply:  'x' or '*'
%  divide:    '÷' or '/'
%
% For example: "7+7÷7+7x7-7"
%
% Output
% ------
%
% Print out all the possible bracketings, one per line.
% If the number of operators is n, then the number of 
% answers will be the Catalan number for n+1:
%
% http://en.wikipedia.org/wiki/Catalan_number
%
% http://oeis.org/A000108 
%
% Example
% -------
%
% > brackets:run( "7+7/7-7" ).
% (7+(7/(7-7)))
% (7+((7/7)-7))
% ((7+7)/(7-7))
% ((7+(7/7))-7)
% (((7+7)/7)-7)
% ok
%
% Where there are 3 operators, and C4 = 5, so 5 possibilities.
%
% The example/0 function is hardcoded with the 5-operator expression:
%          "7+7÷7+7x7-7" 
% which generates C6 = 42 lines of output.
% 
% Solution
% --------
%
% A simple recursive decomposition of the expression:
%   - iterate over all operators to select a root
%   - for each root:
%       - divide the expression at the chosen operator
%         to give left and right sub-expressions
%       - decompose those expressions recursively
%       - write each possibility with bracketed terms
%
% The solution is written in an imperative style,
% with indexed access to lists, as if they were arrays.
% However, because the output increases so rapidly with n,
% it is expected that all the input strings will be quite short.
% The final string concatenation uses the inefficient ++ operator,
% but again, the strings are short, so it should not be a problem.
% We could use [$(|Left] ++ [Op|Right] ++ ...
% but I think ++ is much clearer than list construction.
%
% -----------------------------------------------------

example() -> run( "7+7÷7+7x7-7" ).

run( Exp ) -> [ io:format("~s~n",[B]) || B <- brackets(Exp) ], ok.
  
brackets( Exp ) -> 
  case length(Exp) of
    1 -> [Exp];     
    N -> [ "(" ++ Left ++ [Op] ++ Right ++ ")" || 
            I     <- seq( 2, N-1, 2 ),
            Op    <- [ nth( I, Exp ) ],
            Left  <- brackets( sublist( Exp,   1, I-1 ) ), 
            Right <- brackets( sublist( Exp, I+1, N-I ) )  
         ]
  end.


