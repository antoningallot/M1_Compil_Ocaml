%{

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST
  
%}

(* Définition des lexèmes *)
%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP
%token BREAK CONTINUE

%token VAR
%token INTEGER BOOLEAN

%token MAIN
%token IF ELSE WHILE FOR
%token SEMI COMMA
%token SET PRINT
%token BEGIN END
%token EOF

(* Définition du symbole initial *)
%start prog
%type <SourceLocalisedAST.program> prog

%%

(* Symbole non-terminal principal [prog] *)
prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| vars=var_decls; main=main; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { { main = main;
      globals = Symb_Tbl.add "arg" TypInt vars; } }
  
(* Aide : ajout d'une règle pour récupérer grossièrement les erreurs se 
   propageant jusqu'à la racine. *)
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

(* Séquence de déclaration de variables *)
var_decls:
(* Si pas de déclaration, on renvoie la table vide. *)
| (* empty *)  { Symb_Tbl.empty }
(* Sinon : à compléter ! *)
;

(* Bloc de code principal, formé du mot-clé [main] suivi par le bloc
   proprement dit. *)
main:
| MAIN; i=block { i }
;

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_instruction; END { i }
;

(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_instruction:
| i=instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

(* Instructions *)
instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { Nop }
(* Sinon : à compléter ! *)
;
