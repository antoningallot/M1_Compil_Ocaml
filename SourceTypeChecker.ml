open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)

let rec typecheck_instruction context i = match i.instr with
  | Nop -> ()

  | _ -> failwith "Not implemented"
    
let extract_context p =
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
