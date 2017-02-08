{
  open Parser
  exception Eof
}

rule token = parse
  | '~'   { NOT }
  | '-'   { VNOT }
  | "\\/" { OR }
  | "/\\" { AND }
  | 'X'   { XOR }
  | "=>"  { IMPLIES }
  | "<=>" { EQUIV }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | ['0'-'9']+ as s { VAR (int_of_string s) }
  | eof   { raise Eof }
