open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)

let type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool

let check_type context i e ty =
  let t = type_expression context e in
  if t = ty
  then ()
  else raise (Type_error(ty, t, i.i_pos))
    
let rec type_expression context e = match e.expr with
  | Literal a ->  type_literal a
  | Location(Identifier(Id name)) -> Symb_Tbl.find name context.identifier_types
  | UnaryOp (a, b) ->
     match a with
     | Minus -> check_type context e b TypInt
     | Not -> check_type context e b TypBool
  |
       
  | BinaryOp (a, b, c) -> 
      
let rec typecheck_instruction context i = match i.instr with
  | Print e -> check_type context i e TypInt
  | Nop -> ()
     
let extract_context p =
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
