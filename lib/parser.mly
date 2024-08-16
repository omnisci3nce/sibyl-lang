/* Declarations */

%{
  open Ast
%}

%token Eof
%token Newline
%token Let
%token Fn

%token False
%token True
%token If
%token Then
%token Else
%token Print
%token <string> Ident
%token <int> Int
%token <float> F32

%token LParen
%token RParen
%token LBrace
%token RBrace
%token LBracket
%token RBracket
%token Dot
%token Comma
%token Colon
%token Semicolon
%token Plus
%token Minus
%token Star
%token Slash
%token Bang
%token Equal
%token EqualEqual
%token BangEqual
%token LT
%token GT
%token LTE
%token GTE

%start <Ast.program> prog

%%
/* Grammar */

expr:
  | i = Int; { Int i }

stmt:
  | Let; var_name = Ident; Equal; bound_expr = expr
    { Let {
        loc = $startpos;
        var_name = var_name;
        bindee = bound_expr
      }
    }

toplevel_item:
  | stmt = stmt { Stmt stmt }

prog:
  | prog = separated_list(Newline, toplevel_item); Eof { prog }
