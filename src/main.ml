open Expr

let compile e = begin
  let bdd, n_vars, n_nodes = Bdd.from_expr e in
  Bdd.display bdd;
  print_endline @@ string_of_expr e;
end

(* global parameters *)
let source_file = ref ""
let export_file = ref ""
let use_minisat = ref false
let use_debug = ref false

let speclist =
  [ "-tseitin", Arg.Set_string export_file, "Export the given formula in SAT format"
  ; "-minisat", Arg.Set use_minisat, "Run minisat on the given formula"
  ; "-debug", Arg.Set use_debug, "Print additionnal information during execution"
  ]

let () = begin
  Arg.parse speclist ((:=) source_file) "F2BDD 2017";

  compile @@ Test.drawers 5;

  (* todo : improve & handle multiple errors *)
  try
    open_in !source_file
    |> Lexing.from_channel
    |> Parser.main Lexer.token
    |> compile
  with _ ->
     print_string @@ "Error while parsing file " ^ !source_file ^ "\n"
end
