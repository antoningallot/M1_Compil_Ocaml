open CommonAST

type expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * expression
  | BinaryOp of binaryOp * expression * expression

and location =
  | Identifier  of identifier


type instruction =
  | Print       of expression
  | Set         of location   * expression
  | Conditional of expression * instruction * instruction
  | Loop        of expression * instruction
  | Sequence    of instruction * instruction
  | Nop

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
}
