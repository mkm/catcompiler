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
type t__10__ = string*(int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = int*(int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
in
datatype token =
    ARROW of t__1__
  | BAR of t__2__
  | BOOL of t__3__
  | COLON of t__4__
  | ELSE of t__5__
  | END of t__6__
  | EOF of t__7__
  | EQUAL of t__8__
  | FUN of t__9__
  | ID of t__10__
  | IF of t__11__
  | INT of t__12__
  | LESS of t__13__
  | LPAR of t__14__
  | MATCHARROW of t__15__
  | MINUS of t__16__
  | NUM of t__17__
  | PLUS of t__18__
  | READ of t__19__
  | RPAR of t__20__
  | THEN of t__21__
  | WRITE of t__22__
end;

