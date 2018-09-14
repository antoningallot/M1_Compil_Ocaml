module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_instruction = function
  | Imp.Sequence (a, b) -> Gto.Sequence(translate_instruction a, translate_instruction b)
  | Imp.Print a -> Gto.Print(translate_expression a)
  | Imp.Set (a, b) -> Gto.Set(translate_location a, translate_expression b)
  | Imp.Conditional (e, i1, i2) ->
     let then_label = new_label()
     and else_label = new_label()
     and end_label = new_label()
     in
     Gto.ConditionalGoto(then_label, translate_expression e)
     ++ translate_instruction(i2) (* Code du bloc else *)
     ++ Gto.Goto(end_label) (* Fin du bloc then, aller fin *)
     ++ Gto.Label(then_label) (* Bloc then *)
     ++ translate_instruction(i1) (* Code du bloc then *)
     ++ Gto.Label(end_label)
       
  | Imp.Loop (a, b) -> Gto.Goto(new_label ()) (* Refaire faux !!!! *)
  | Imp.Nop -> Gto.Nop
and translate_expression = function
  | Imp.Literal a -> Gto.Literal(a)
  | Imp.Location a -> Gto.Location(translate_location a)
  | Imp.UnaryOp (a, b) -> Gto.UnaryOp(a, translate_expression b)
  | Imp.BinaryOp (a, b, c) -> Gto.BinaryOp(a, translate_expression b, translate_expression c)
and translate_location = function
  | Imp.Identifier id -> Gto.Identifier(id)
     
let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
