module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

let rec strip_instruction i = match Src.(i.instr) with
  | Src.Print a -> Imp.Print(strip_expression a)
  | Src.Set (a, b)-> Imp.Set(strip_location a, strip_expression b)
  | Src.Conditional (a, b, c) -> Imp.Conditional(strip_expression a, strip_instruction b, strip_instruction c) 
  | Src.Loop (a, b) -> Imp.Loop(strip_expression a, strip_instruction b) 
  | Src.Sequence (a, b) -> Imp.Sequence(strip_instruction a, strip_instruction b) 
  | Src.Nop -> Imp.Nop
and strip_expression i = match Src.(i.expr) with
  | Src.Literal a -> Imp.Literal(a)
  | Src.Location a -> Imp.Location(strip_location a)
  | Src.UnaryOp (a, b) -> Imp.UnaryOp(a, strip_expression b)
  | Src.BinaryOp (a, b, c) -> Imp.BinaryOp(a, strip_expression b, strip_expression c)
and strip_location i = match i with
  | Src.Identifier a -> Imp.Identifier(a)
      
let strip_program p =
  let main = strip_instruction Src.(p.main) in
  let globals = Src.(p.globals) in
  Imp.({ main; globals; })
