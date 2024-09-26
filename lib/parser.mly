/* Declarations */

%{
  open Ast
%}

%token Eof
%token Newline
%token Let
%token FuncDecl

%token False
%token True
%token If
%token Then
%token Else
%token Print
%token Return
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
%token Tilde
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
  | Tilde; e = expr { UnaryOp { rhs = e; operator = Complement } }
  | Minus; e = expr { UnaryOp { rhs = e; operator = Negate } }
  | LParen; e = expr; RParen { e }

stmt:
  | Let; var_name = Ident; Equal; bound_expr = expr
    { Let {
        loc = $startpos;
        var_name = var_name;
        bindee = bound_expr
      }
    }
  | Return; return_expr = expr
    {
        Return { value = return_expr }
    }

toplevel_item:
  | stmt = stmt { Stmt stmt }
  | FuncDecl; func_name = Ident; LParen; RParen; LBrace; body = list(stmt); RBrace
    { FuncDecl {
        loc = $startpos;
        func_name = func_name;
        arity = 0;
        body = body
      }
    }

prog:
  | prog = separated_list(Newline, toplevel_item); Eof { prog }
