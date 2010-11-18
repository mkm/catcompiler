structure  Cat =
struct

  (* types for abstract syntax for Cat *)

  type pos = int * int  (* position: (line, column) *)

  datatype Type
    = Int of pos
    | Bool of pos
    | TyVar of string * pos

  datatype Pat
    = NumP of int * pos
    | TrueP of pos
    | FalseP of pos
    | NullP of pos
    | VarP of string * pos
    | TupleP of Pat list * pos

  datatype Exp 
    = Num of int * pos
    | True of pos
    | False of pos
    | Null of string * pos
    | Var of string * pos
    | Plus of Exp * Exp * pos
    | Minus of Exp * Exp * pos
    | Equal of Exp * Exp * pos
    | Less of Exp * Exp * pos
    | Not of Exp * pos
    | And of Exp * Exp * pos
    | Or of Exp * Exp * pos
    | Let of Dec * Exp * pos
    | If of Exp * Exp * Exp * pos
    | MkTuple of Exp list * string * pos
    | Case of Exp * Match * pos
    | Apply of string * Exp * pos
    | Read of pos
    | Write of Exp * pos

  withtype Match = (Pat * Exp) list

  and Dec = (Pat * Exp * pos) list

  type TyDec = string * Type list * pos

  type FunDec = string * Type * Type * Match * pos

  type Prog = TyDec list * FunDec list * Exp

end
