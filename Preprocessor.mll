{
  (* Contexte *)
  open Lexing
  let macros = ref [] (* Liste de tuples (macro_name, macro_text) *)
  let temp_name = ref ""
  let f = open_in "tests/macro.cid"
  let output = open_out "tests/macro.pp.cid"
  let print_file f s = Printf.fprintf f s

  let find_macro l =
    (* Définition d'une table des mots-clés et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      l ;
    fun s ->
      (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
         on le renvoie. *)
      try  Hashtbl.find h s
      (* Et sinon on considère qu'il s'agit d'un identifiant. *)
      with Not_found -> ""
}

(* Raccourci : caractères alphabétiques *)
let alpha = ['a'-'z' 'A'-'Z']

(* Expressions régulières définissant les lexèmes *)
rule start = parse
  | "#DEFINE "
      { print_file output "%s" (lexeme lexbuf); macro_name lexbuf }
  | '\n'|' '|'\t'
      { print_file output "%s" (lexeme lexbuf); start lexbuf }
  (* Fin des "define" *)
  | _
      { print_file output "%s" (lexeme lexbuf); code lexbuf }
and macro_name = parse
    | alpha+" "
	{ print_file output "%s" (lexeme lexbuf); temp_name := (lexeme lexbuf) ;
	  macro_text lexbuf }
and macro_text = parse
    | [^'\n']+'\n'
	{ print_file output "%s" (lexeme lexbuf); macros := (!temp_name, (lexeme lexbuf))::(!macros); start lexbuf }
    | eof
	{ failwith ("define not finished before end of file") }
and code = parse
    | alpha+ as s
	{ let text = find_macro (!macros) s in
	  if text = ""
	  then print_file output "%s" s
	  else print_file output "%s" text;
	  code lexbuf
	}	    
    | eof
	{ close_in f; close_out output }
    | _
	{ print_file output "%s" (lexeme lexbuf); code lexbuf }


	{ let lb = Lexing.from_channel f in
	start lb }
