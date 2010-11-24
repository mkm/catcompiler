local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = string*(int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = int*(int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
in
datatype token =
    ARROW of t__1__
  | BAR of t__2__
  | BOOL of t__3__
  | COLON of t__4__
  | END of t__5__
  | EOF of t__6__
  | EQUAL of t__7__
  | FUN of t__8__
  | ID of t__9__
  | INT of t__10__
  | LESS of t__11__
  | LPAR of t__12__
  | MATCHARROW of t__13__
  | MINUS of t__14__
  | NUM of t__15__
  | PLUS of t__16__
  | READ of t__17__
  | RPAR of t__18__
  | WRITE of t__19__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Cat.Prog;
