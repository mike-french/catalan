# Bracketings of Arithmetic Expressions

## Problem

Given a simple flat arithmetic expression containing digits and binary operators, generate all possible bracketings of terms and (optionally) evaluate them. The problem is equivalent to finding all possible binary trees over a set of leaf nodes.

## Input

A string of interleaved single-digit non-zero integers `1-9` and binary arithmetic operators, as follows:

| Operation | Character |
|---|---|
| add       | `+` |
| subtract  | `-` |
| multiply  | `x` or `*` |
| divide    | `÷` or `/` |

For example: `7+7÷7+7x7-7`

Note that spaces are not allowed.  
No checks are performed to test if the input is well-formed.

## Output
 
Print all the possible bracketings, one per line.
Optionally append an equals sign and the result of evaluating the expression, or `NaN` if there is a _Divide By Zero_ error.
 
 If the number of operators is n, then the number of lines of output will be the Catalan Number for n+1:
 - Catalan Number \[[wikipedia](http://en.wikipedia.org/wiki/Catalan_number)\]
 - Online Encyclopedia of Integer Sequences \[[A000108](http://oeis.org/A000108)\]
 
| n | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|----|
| Cn | 1 | 1 | 2 | 5 | 14 | 42 | 132 | 429 | 1430 | 4862 |

## Example
 
For expression `7+7/7-7` there are 3 operators and C4 = 5, so 5 possibilities:
```
(7+(7/(7-7))) 
(7+((7/7)-7)) 
((7+7)/(7-7)) 
((7+(7/7))-7)
(((7+7)/7)-7) 
```

The `example/0` function is hardcoded with the 5-operator expression: `"7+7÷7+7x7-7"`   
which generates C6 = 42 lines of output.
 
## Solutions
 
### Simple Imperative
 
The `brackets` module contains a simple 10-line solution to print all the bracketings.
 
A simple recursive decomposition of the expression:
- Iterate over all operators to select a root.
- For each root:
  - Divide the expression at the chosen operator to give left and right sub-expressions.
  - Decompose the left and right sub-expressions recursively.
  - Write each possibility with brackets around each operator expression.

The code is not performance critical, so clarity beats efficiency. The solution is written in an imperative style, with indexed access to lists, as if they were arrays (slow). The final string concatenation uses the `++` operator (slow). We could use list construction, such as `[$(|Left] ++ [Op|Right] ++ ...`, but I think `++` is much clearer. 

#### Usage:

At the Erlang shell:
```
> c(brackets).
> brackets:run("7+7/7-7"). 
 (7+(7/(7-7))) 
 (7+((7/7)-7)) 
 ((7+7)/(7-7)) 
 ((7+(7/7))-7) 
 (((7+7)/7)-7) 
 ok
> brackets:example().
 ... 
ok
 ```

### Recursive AST
 
The `catalan` module contains a multi-stage solution to print all the bracketings and evaluate the results.
 
The solution comprises:
- Parse the input string to create a list of tokens (input AST).
- For each arithmetic operator: make a rooted term of the form `{ left-arg, operator, right-arg }` 
- For each rooted term: expand to make a tree (output AST), by recursively decomposing each right/left subterm, until all branch nodes are operators, and all leaf nodes are single numerical values.
- For each tree, print an output line:
  - Print the bracketed expression recursively. 
  - Evaluate recursively by combining values according to the arithmetic operators in the branch nodes.
  - Print the result value. 

The code also includes Erlang type annotations, which was an interesting exercise in itself.

#### Usage

At the Erlang shell:
```
> c(catalan).
> catalan:run("7+7/7-7"). 
 (7+(7/(7-7))) = NaN
 (7+((7/7)-7)) = 1
 ((7+7)/(7-7)) = NaN
 ((7+(7/7))-7) = 1
 (((7+7)/7)-7) = -5
 ok
 > catalan:example().
 ... 
 ok
 ```
