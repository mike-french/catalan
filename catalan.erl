%% coding: latin-1
%--------------------------------------------------------------------
%
% Copyright (c) 2015 Mike French
%
% This software is released under the MIT license
% http://www.opensource.org/licenses/mit-license.php
%
%--------------------------------------------------------------------

-module( catalan ).

-export( [example/0, run/1] ).

% suppress warning for unused utility function

-export( [serialize/1] ).

% -----------------------------------------------------
%
% Problem
% -------
%
% Given a simple flat arithmetic expression, generate all
% the possible bracketings of terms and evaluate them.
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
% Print out all the possible bracketings, one per line,
% with an equals sign and the result of evaluating the expression.
% If the number of operators is n, then the number of 
% lines of output will be the Catalan number for n+1:
%
% http://en.wikipedia.org/wiki/Catalan_number
%
% http://oeis.org/A000108 
%
% Example
% -------
%
% > catalan:run("7+7/7-7"). 
% (7+(7/(7-7))) = NaN
% (7+((7/7)-7)) = 1
% ((7+7)/(7-7)) = NaN
% ((7+(7/7))-7) = 1
% (((7+7)/7)-7) = -5
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
% The solution comprises:
%  - parse the input string to make a list of tokens (input AST)
%  - make a list of rooted terms of the form {left,op,right}
%    by using each arithmetic operator as a root
%  - expand each rooted term to make a tree (output AST)
%    by recursively decomposing each right/left subterm
%    until all branch nodes are operators
%    and all leaf nodes are single numerical values
%  - for each tree:   
%      - evaluate the tree by recursively combining values
%        according to the arithmetic operator in the branch nodes
%      - print the line of the result
%    
% -----------------------------------------------------
% Types

% expression is the input string

-type expression() :: nonempty_string().

% tokens are the nodes of an AST for the parsed expression

-type digit()  :: 1..9.
-type op()     :: add | sub | mul | mal | dvd | dsd.
-type token()  :: digit() | op().
-type tokens() :: [token(),...].

% root is first split of token list by chosing one op as the root

-type root()  :: { tokens(), op(), tokens() }.  
-type roots() :: [root()].

% tree is a full hierarchy of branch nodes and leaf values
% leaf values can be results of evaluation
% so they include floating point numbers from divisions
% and the 'nan' atom that represents a divide-by-zero error

-type value() :: nan | number().
-type tree()  :: value() | { tree(), op(), tree() }.  
-type trees() :: [tree(),...].

% -----------------------------------------
% main program

-spec example() -> ok.

example() -> run( "7+7÷7+7x7-7" ).

-spec run( expression() ) -> ok.

run( Exp ) -> [ print_eval(Tree) || Tree <- trees( tokenize(Exp) ) ], ok.

% -----------------------------------------
% string to tokens

-spec chr2tok( char() ) -> token().

chr2tok( $+ ) -> add;
chr2tok( $- ) -> sub;
chr2tok( $x ) -> mul;
chr2tok( $* ) -> mal;
chr2tok( $÷ ) -> dvd;
chr2tok( $/ ) -> dsd;
chr2tok( X  ) when is_integer(X) and (X>=$0) and (X=<$9) -> X-$0.

-spec tokenize( expression() ) -> tokens().

tokenize( Exp ) -> [ chr2tok(Chr) || Chr <- Exp ].

% -----------------------------------------
% tokens to string

-spec tok2chr( token() ) -> char().

tok2chr( add ) -> $+;
tok2chr( sub ) -> $-;
tok2chr( mul ) -> $x;
tok2chr( mal ) -> $*;
tok2chr( dvd ) -> $÷;
tok2chr( dsd ) -> $/;
tok2chr(  X  ) when is_integer(X) and (X>=1) and (X=<9) -> X+$0.

-spec serialize( tokens() ) -> expression().

serialize( Tokens ) -> [ tok2chr(Tok) || Tok <- Tokens ].

% -----------------------------------------
% tree to number

-spec eval( tree() ) -> value().

eval( Val ) when is_number(Val) -> Val;
eval( { L, Op, R } ) -> op( eval(L), Op, eval(R) ).

-spec op( value(), op(), value() ) -> value().

op( nan, _, _ ) -> nan;
op( _, _, nan ) -> nan;
op( X, add, Y ) -> X + Y;
op( X, sub, Y ) -> X - Y;
op( X, mul, Y ) -> X * Y;
op( X, mal, Y ) -> X * Y;
op( X, dvd, Y ) -> try (X / Y) catch _:_ -> nan end;
op( X, dsd, Y ) -> try (X / Y) catch _:_ -> nan end.

% -----------------------------------------
% tree to string

-spec str( tree() ) -> expression().

str( { L, Op, R } ) -> "(" ++ str(L) ++ str(Op) ++ str(R) ++ ")";
str( Tok ) -> [tok2chr(Tok)].

% -----------------------------------------
% tokens to roots to trees

-spec trees( tokens() ) -> trees().

trees( Leaf ) when length(Leaf) == 1 -> Leaf;
trees( Tokens ) -> 
  [ { L, Op, R } || 
      { Left, Op, Right } <- roots(Tokens),
      L <- trees( Left ),
      R <- trees( Right )
  ].

% -----------------------------------------
% tokens to roots

-spec roots( tokens() ) -> roots().

roots( [L,Op|Right] ) -> roots( { [L], Op, Right }, [] ).

-spec roots( root(), roots() ) -> roots().

roots( Root={ _, _, [_] }, Roots ) -> 
  lists:reverse( [Root|Roots] );
roots( Root={ Left, Op1, [R,Op2|Right] }, Roots ) -> 
  roots( { Left++[Op1,R], Op2, Right }, [Root|Roots] ).

% -----------------------------------------
% print utilities

-spec print_eval( tree() ) -> ok.

print_eval( Tree ) when is_tuple(Tree) -> 
  io:format( "~s = ~s~n", [str(Tree),pr(eval(Tree))] ).
  
-spec pr( value() ) -> string().

pr( nan ) -> "NaN";
pr( I ) when (I-trunc(I) < 1.0e-5) -> io_lib:format( "~B", [trunc(I)] );
pr( X ) when is_float(X) -> io_lib:format( "~p", [X] ).

% -----------------------------------------
