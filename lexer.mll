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

let character = ['a'-'z']
let range = '[' ['a'-'z'] '-' ['a'-'z'] ']'
let word = '[' ['a'-'z']+ ']'

rule read_token = parse
  | '(' { OPENPAR }
  | ')' { CLOSEPAR }
  | '[' { OPENSQPAR }
  | ']' { CLOSESQPAR }
  | '|' { ALTERNATE }
  | '*' { STAR }
  | '-' { DASH }
  | eof { EOF }
  | character { LETTER (char_of_string (Lexing.lexeme lexbuf)) }
  | range { read_range }
  | word { read_word }
  | _ { Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0))) }

and read_range = parse
  | '[' l1 '-' l2 ']' { RANGE l1 ^ l2 }

and read_word = parse
  | '[' w ']' { WORD w }