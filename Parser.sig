local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = string*(int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = int*(int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
in
datatype token =
    AND of t__1__
  | ARROW of t__2__
  | BAR of t__3__
  | BOOL of t__4__
  | COLON of t__5__
  | ELSE of t__6__
  | END of t__7__
  | EOF of t__8__
  | EQUAL of t__9__
  | FUN of t__10__
  | ID of t__11__
  | IF of t__12__
  | IN of t__13__
  | INT of t__14__
  | LESS of t__15__
  | LPAR of t__16__
  | MATCHARROW of t__17__
  | MINUS of t__18__
  | NOT of t__19__
  | NUM of t__20__
  | OR of t__21__
  | PLUS of t__22__
  | READ of t__23__
  | RPAR of t__24__
  | THEN of t__25__
  | WRITE of t__26__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Cat.Prog;
