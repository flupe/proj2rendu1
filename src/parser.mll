{
  open Parser
  exception Eof
}

rule token = parse
  | '~'   { NOT }
  | "\/"  { OR }
  | "/\\" { AND }
  | 'X'   { XOR }
  | "=>"  { IMPLIES }
  | "<=>" { EQUIV }
  | '('   { LPAREN }
  | ')'   { RPAREN }
