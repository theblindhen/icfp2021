module While

type BinOp =
  | Plus | Minus | Divide | Multiply | Equals | NotEquals | Lteq | Gteq | Lt | Gt
  with
  member this.toString = 
    match this with
      | Plus -> " + "
      | Minus -> " - "
      | Divide -> " / "
      | Multiply -> " * "
      | Equals -> " = "
      | NotEquals -> " != "
      | Lteq -> " <= "
      | Gteq -> " >= "
      | Lt -> " < "
      | Gt -> " > "

type Expr = 
  | Var of string 
  | Int of int
  | Uminus of Expr
  | BinOp of BinOp * Expr * Expr
  with
  member this.toString =
    match this with
      | Var x -> x
      | Int x -> x.ToString()
      | Uminus x -> $"-{x}"
      | BinOp (op,x,y) -> $"({x} {op} {y})"

type Stmt = 
  | Decl of string
  | Assign of string * Expr
  | Scope of Stmt
  | While of Expr * Stmt
  | Seq of Stmt * Stmt
  | If of Expr * Stmt * Stmt option
  | Write of Expr
  | Read of string
  with
  member this.toString =
   match this with
      | Decl x -> $"let {x} = "
      | Assign (x,y) -> $"{x} = {y}"
      | Scope s -> $"{s}"
      | While (x,s) -> $"while {x} do {{ {s} }}"
      | Seq (s,t) -> $"{s};\n{t}"
      | If (x,s,t) -> $"if {x} then {{ {s} }} else {{ {t} }}"
      | Write x -> $"write {x}"
      | Read x -> $"read {x}"

type Prog = Stmt