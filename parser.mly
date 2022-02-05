%start main

%token <char> LETTER
%token <string> RANGE
%token <string> WORD
%token OPENPAR CLOSEPAR OPENSQPAR CLOSESQPAR ALTERNATE STAR DASH EOF

%start prog

%%

prog:
  | e = expr { e }

expr: 
  | e1=expr ALTERNATE e2=expr { Alternate(e1, e2) }
  | e1=expr e2=expr { Concat(e1, e2) }
  | e1=expr STAR { Star(e1) }
  | OPENPAR e=expr CLOSEPAR { Par(e) }
  | l=LETTER { Letter(l) }
  | r=RANGE { Range(r) }
  | w=WORD { Word(w) }