%{
  open Expr
%}

/* declarations */
%token <bool> BOOL
%token NOT OR AND IMPLIES XOR EQUIV
%token LPAREN RPAREN
%token EOL

%left AND
%left OR
%left XOR
%left IMPLIES
%left EQUIV

%start main
%type <Expr.expr> main

%%
main:
  expr EOL { $1 }
;

expr:
  | NOT expr { Not $2 }
  | BOOL { Const $1 }
  | expr AND expr { And ($1, $3) }
  | expr OR expr { Or ($1, $3) }
  | expr XOR expr { Xor ($1, $3) }
  | expr IMPLIES expr { Implies ($1, $3) }
  | expr EQUIV expr { Equiv ($1, $3) }
  | LPAREN expr RPAREN { $2 }
;

