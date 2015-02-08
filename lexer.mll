{
  open Parser
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let const = ['a'-'z'] ['_' '-' 'a'-'z' 'A'-'Z' '0'-'9']*
let var = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let nbr = ['0' - '9']*


rule token = parse
    '#' [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | "$use"          { USE }
  | "$quit"         { QUIT }
  | "?-"            { GOAL }
  | ":-"            { FROM }
  | ";;"            { SEMICOLON2 }
  | "true"          { TRUE }
  | "drs"           { DRS }
  | "=&gt;"         { IMPLY }
  | "MUST"          { MUST }
  | "must"          { MUST }
  | "MAY"           { MAY }
  | "may"           { MAY }
  | "can"           { CAN }
  | "CAN"           { CAN }
  | "COMMAND"       { COMMAND }
  | "command"       { COMMAND }
  | "v"             { UNION }
  | "list"          { LIST } 
  | '\"' [^'\"']* '\"' { let str = lexeme lexbuf in STRING (String.sub str 1 (String.length str - 2)) }
  | '\'' [^'\'']* '\'' { let str = lexeme lexbuf in STRINGALL (String.sub str 1 (String.length str - 2)) }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '['             { GCROCHET}
  | ']'             { DCROCHET}
  | '.'             { POINT }
  | '-'             { TIRET }
  | '/'             { SLASH }
  | '\''            { QUOTE }
  | '~'             { NAF }
  | ':'             { DEUXPOINT } 
  | nbr             { NBR (int_of_string (lexeme lexbuf))}
  | const           { CONST (lexeme lexbuf) }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}
