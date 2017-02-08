open Expr

let compile e = begin
  print_endline @@ string_of_expr e
end

let lexbuf = Lexing.from_channel stdin

let parse () =
  Parser.main Lexer.token lexbuf

let calc () =
  try
    let res = parse () in
    compile res;
    flush stdout
  with _ ->
    print_string "erreur de saisie\n"

let _ = calc ()
