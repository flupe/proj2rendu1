open Expr

let source_file = ref ""
let export_file = ref ""
let use_minisat = ref false
let use_debug = ref false
let use_tests = ref false

let speclist =
  [ "-tseitin", Arg.Set_string export_file, "Export the given formula in SAT format"
  ; "-minisat", Arg.Set use_minisat, "Run minisat on the given formula"
  ; "-debug", Arg.Set use_debug, "Print additionnal information during execution"
  ; "-tests", Arg.Set use_tests, "Runs a full test suite"
  ]

let print_header s =
  print_string "/";
  for i = 1 to String.length s + 2 do
    print_string "="
  done;
  print_string "\\";
  print_newline ();
  print_string "| ";
  print_string s;
  print_string " |";
  print_newline ();
  print_string "\\";
  for i = 1 to String.length s + 2 do
    print_string "="
  done;
  print_string "/";
  print_newline ()

let () = begin
  Arg.parse speclist ((:=) source_file) "F2BDD 2017";
  
  if !use_tests then begin
    print_header "Will it blend? That is the question.";
    Tests.run ();
    exit 0
  end;

  let in_chan =
    try open_in !source_file
    with _ ->
      print_header "Houston, we have a situation.";
      print_string @@ "File `" ^ !source_file ^ "` can't be opened.\n";
      exit 1 in

  let expr =
    try 
      Lexing.from_channel in_chan
      |> Parser.main Lexer.token
    with _ ->
      print_header "Houston, we have a situation.";
      print_string @@ "There was an error while parsing `" ^ !source_file ^ "`.\n";
      exit 1 in

  let cnf =
    try
      if !export_file <> "" || !use_minisat then
        Tseitin.transform expr
      else 
        Cnf.create ()
    with _ ->
      print_header "Houston, we have a situation.";
      print_string @@ "There was an error during the Tseitin transform.\n";
      exit 1 in

  let bdd, n_vars, n_nodes = 
    try
      Bdd.from_expr expr
    with _ ->
      print_header "Houston, we have a situation.";
      print_string @@ "There was an error while building the BDD.\n";
      exit 1 in

  if !use_debug then begin
    print_header "Just a bit of debug information.";
    print_endline @@ "Parsed expression: " ^ (string_of_expr expr);
    print_string "Generated CNF: ";
    Cnf.print cnf;
    Bdd.display bdd;
    print_newline ();
    print_newline ();
  end;

  print_header "What the generated BDD looks like.";
  print_endline @@ "Number of variables: " ^ (string_of_int n_vars);
  print_endline @@ "Number of nodes: " ^ (string_of_int n_nodes);

  if !use_minisat then begin  
    print_newline ();
    print_header "What `minisat` has to say.";
    match Cnf.minisat cnf with
      | None -> print_endline "ARGH. The formula isn't satisfiable."
      | Some assign -> begin
          print_endline "OK. The formula is satisfiable.";
          print_newline ();
          print_endline "Possible assignations:";
          Hashtbl.iter (fun v b ->
            if b then
              print_endline @@ "- Variable " ^ string_of_int v ^ " is true."
            else
              print_endline @@ "- Variable " ^ string_of_int v ^ " is false.") assign
        end
  end;

  if !export_file <> "" then begin
    try
      let out_chan = 
        open_out !export_file in
      Cnf.dimacs cnf
      |> output_string out_chan;
      flush out_chan
    with _ ->
      print_header "Houston, we have a situation.";
      print_string @@ "File `" ^ !export_file ^ "` can't be opened.\n";
      exit 1 
  end;

  ()
end