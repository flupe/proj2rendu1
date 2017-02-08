%{
  open Expr
%}

/* declarations */
%token <int> VAR
%token NOT VNOT OR AND XOR IMPLIES EQUIV
%token LPAREN RPAREN
%token EOL

%left IMPLIES EQUIV
%left XOR
%left OR
%left AND

%nonassoc NOT
%nonassoc VNOT

%start main
%type <Expr.expr> main

%%
main:
  expr EOL { $1 }
;

expr:
  | VAR { Var $1 }
  | VNOT VAR { Not (Var $2) }
  | NOT expr { Not $2 }
  | LPAREN expr RPAREN { $2 }
  | expr AND expr { And ($1, $3) }
  | expr OR expr { Or ($1, $3) }
  | expr XOR expr { Xor ($1, $3) }
  | expr IMPLIES expr { Implies ($1, $3) }
  | expr EQUIV expr { Equiv ($1, $3) }
;

