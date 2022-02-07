%{
open Ast
%}

%token <int> INT
%token EOF

%start <Ast.expr> prog

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | i = INT { Int i }
    ;