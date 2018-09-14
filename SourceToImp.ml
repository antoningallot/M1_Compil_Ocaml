module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

let rec strip_instruction i = match Src.(i.instr) with
  | Src.Nop ->
    Imp.Nop

  | _ -> failwith "Not implemented"
      
let strip_program p =
  let main = strip_instruction Src.(p.main) in
  let globals = Src.(p.globals) in
  Imp.({ main; globals; })
