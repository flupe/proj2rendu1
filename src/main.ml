open Expr

let compile e = begin
  let bdd = Bdd.from_expr e in
  let soi = string_of_int in
  List.iter (fun (k, (v, l, h)) ->
    print_endline @@ soi k^": "^soi v^", "^soi l^", "^soi h
  ) bdd;
  print_endline @@ string_of_expr e;
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
