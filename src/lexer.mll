{
  open Parser
  exception Eof
}

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | '~'    { NOT }
  | '-'    { VNOT }
  | ['0' '\n'] { EOL }
  | "\\/"  { OR }
  | "/\\"  { AND }
  | 'X'    { XOR }
  | "=>"   { IMPLIES }
  | "<=>"  { EQUIV }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | ['0'-'9']+ as s { VAR(int_of_string s) }
  | eof    { raise Eof }
