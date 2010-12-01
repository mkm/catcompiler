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
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = string*(int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = int*(int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = (int*int)
type t__32__ = (int*int)
in
datatype token =
    AND of t__1__
  | ARROW of t__2__
  | AT of t__3__
  | BAR of t__4__
  | BOOL of t__5__
  | CASE of t__6__
  | COLON of t__7__
  | COMMA of t__8__
  | ELSE of t__9__
  | END of t__10__
  | EOF of t__11__
  | EQUAL of t__12__
  | FUN of t__13__
  | ID of t__14__
  | IF of t__15__
  | IN of t__16__
  | INT of t__17__
  | LESS of t__18__
  | LET of t__19__
  | LPAR of t__20__
  | MATCHARROW of t__21__
  | MINUS of t__22__
  | NOT of t__23__
  | NUM of t__24__
  | OF of t__25__
  | OR of t__26__
  | PLUS of t__27__
  | READ of t__28__
  | RPAR of t__29__
  | SEMICOLON of t__30__
  | THEN of t__31__
  | WRITE of t__32__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Cat.Prog;
