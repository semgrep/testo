/*
   Ocamlyacc grammar to parse boolean combinations of tags

   Sample input: (foo or bar) and not e2e

   We stick to ocamlyacc rather than using menhir because the grammar
   is simple and we prefer to minimize package dependencies.
*/

%token <Tag.t> TAG
%token AND OR NOT LPAREN RPAREN EOF
%left OR          /* lowest precedence */
%left AND
%nonassoc NOT     /* highest precedence */
%start main
%type <Tag.query> main
%%
main:
    expr EOF              { $1 }
;
expr:
  | TAG                   { Has_tag $1 }
  | LPAREN expr RPAREN    { $2 }
  | expr OR expr          { Or ($1, $3) }
  | expr AND expr         { And ($1, $3) }
  | NOT expr              { Not $2 }
;
