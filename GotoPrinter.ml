open Printf
open GotoAST
open CommonAST
  
let print_literal = function
  | Int i -> sprintf "%d" i
  | Bool true -> "true"
  | Bool false -> "false"
  
let print_uop = function
  | Minus -> "-"
  | Not -> "!"

let print_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
  
let rec print_expression = function
  | Literal lit ->
    print_literal lit
      
  | Location loc ->
    print_location loc
      
  | UnaryOp(uop, e) ->
    sprintf "%s%s" (print_uop uop) (print_expression e)
      
  | BinaryOp(bop, e1, e2) ->
    sprintf "(%s %s %s)" (print_expression e1) (print_bop bop) (print_expression e2)

and print_location = function
  | Identifier(Id id) -> id
    
let rec print_instruction = function
  | Sequence(i1, i2) ->
    sprintf "%s\n%s" (print_instruction i1) (print_instruction i2)
      
  | Print(e) ->
    sprintf "  print(%s)" (print_expression e)
      
  | Set(loc, e) ->
    sprintf "  %s := %s" (print_location loc) (print_expression e)
      
  | Label(Lab l) ->
    sprintf "%s:" l
      
  | Goto(Lab l) ->
    sprintf "  goto(%s)" l
      
  | ConditionalGoto(Lab l, e) ->
    sprintf "  goto-when(%s, %s)" l (print_expression e)
      
  | Nop ->
    sprintf "  nop"

let print_vars tbl =
  Symb_Tbl.fold (fun id _ code -> sprintf "%s\n%s" id code) tbl ""

let print_program fmt p =
  fprintf fmt "%s\n" (print_vars p.globals);
  fprintf fmt "%s" (print_instruction p.main)
  (* sprintf "%s\n\n%s" (print_vars p.globals) (print_instruction p.main) *)
  
