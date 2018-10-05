{
  (**
     1-  output en @param des regles
     2-  Str chargement forcé ? Comment fonctionne le module / ou existe t'il un autre moyen ?
     3-  Bcp de ref, est-ce grave ?
     4-  Peut_on isolé des partie d'une expr régulière dans des variable (plutot que ... as ... as ... as ... as etc) ?
     5- doit-on gérer l'inception ?
     6- les #DEFINE doivent-ils apparaitrent dans le .pp.cid ?
  **)
  (* Contexte *)
  open Lexing
  let macros = Hashtbl.create 20
  let temp_key = ref ("",0)
  let temp_name = ref ""
  let temp_args = ref []
  let print_file f s = Printf.fprintf f s

  let find_macro s =
    (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
       on le renvoie. *)
    try  Hashtbl.find macros s
    (* Le nom de macro recherché n'a pas été définie *)
    with Not_found -> failwith ("unknown macro name")

  let replace_args s l =
    let rec replace_args_rec l n res =
      match l with
      | [] -> res
      | ss::next -> 
            let temp_txt = "#"^string_of_int(n) in
            replace_args_rec next (n+1) (Str.global_replace (Str.regexp temp_txt) ss res)   
    in
    replace_args_rec l 1 s
}

(* Raccourci : caractères alphabétiques *)
let alpha = ['a'-'z' 'A'-'Z']
(* Raccourci : nombres entiers décimaux *)
let number = ['1'-'9']['0'-'9']* | '0'

(* Expressions régulières définissant les lexèmes *)
rule read output = parse
  (* On repère une définition de macro *)
  | "#DEFINE "
      { macro_name output lexbuf }
  (* On repère une macro dans la partie main *)
  | "#"
      { macro_search_name output lexbuf }
  | '\n'|' '|'\t'
      { print_file output "%s" (lexeme lexbuf); read output lexbuf }
  | eof
      { close_out output }
  | _
      { print_file output "%s" (lexeme lexbuf); read output lexbuf }
and macro_name output = parse
    | (alpha+ as name)
     "{"
      (number as nb)
     "}"
	{ temp_key := (name, int_of_string(nb)) ;
	  macro_name output lexbuf }
    | alpha+
	{ temp_key := ((lexeme lexbuf),0) ;
	  macro_name output lexbuf }
    |" "
	{ macro_text output lexbuf }
    | _
	{ failwith ("Invalide macro definition")}
and macro_text output = parse
    | [^'\n']+
	{ Hashtbl.add macros !temp_key (lexeme lexbuf); macro_text output lexbuf }
    | '\n'
	{ read output lexbuf }
    | eof
	{ failwith ("Define not finished before end of file") }
and macro_search_name output = parse
    | alpha+
	{ temp_name := lexeme lexbuf;
	  macro_search_args output lexbuf
	}
and macro_search_args output = parse
    |'{'
      ([^'}']* as arg)
     '}'
	{ temp_args := arg::!temp_args;
	  macro_search_args output lexbuf }
    |_
	{
	  let text = find_macro (!temp_name,List.length !temp_args) in
	  if List.length !temp_args > 0
	  then
	    begin
	      temp_args := List.rev !temp_args;
	      print_file output "%s" (replace_args text !temp_args)
	    end
	  else
	    print_file output "%s" text;
	  temp_args := [];
	  print_file output "%s" (lexeme lexbuf);
	  read output lexbuf;
	}
       


{
  let preprocessor file lb =
    let output = open_out file in
    read output lb
}
