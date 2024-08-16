{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0'-'9']
let digits =  digit*
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let int = digits

rule read =
  parse
  | whitespace { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { Int (int_of_string (Lexing.lexeme lexbuf))}
  | "let" { Let }
  | "if" { If }
  | "then" { Then }
  | "else" { Else }
  | "print" { Print }
  | ident { Ident (Lexing.lexeme lexbuf) }
(*  | "fn" { FuncDecl } *)
  | '=' { Equal }
  | '(' { LParen }
  | ')' { RParen }
  | '[' { LBracket }
  | ']' { RBracket }
  | '{' { LBrace }
  | '}' { RBrace }
  | '.' { Dot }
  | ',' { Comma }
  | ':' { Colon }
  | ';' { Semicolon }
  | '+' { Plus }
  | '-' { Minus }
  | '*' { Star }
  | '/' { Slash }
  | '!' { Bang }
  | '=' { Equal }
  | "==" { EqualEqual }
  | "!=" { BangEqual }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | eof { Eof }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
