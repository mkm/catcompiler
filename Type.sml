structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype CType = Int | Bool | TyVar of string

  type TTable = (string * CType list) list

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table

  (* combine two symbol tables and check for duplicates *)
  fun combineTables [] table2 p = table2
    | combineTables ((x,v)::table1) table2 p =
        case lookup x table2 of
          SOME _ => raise Error ("Repeated identifier "^x,p)
        | NONE => (x,v) :: combineTables table1 table2 p

  fun checkDups table =
      let
          fun go [] = ()
            | go ((nameA, _)::xs) = (
              map (fn (nameB, _) => if nameA = nameB then raise Error ("name collision", (0, 0)) else ()) xs; go xs
              )
      in
          table
      end

  (* check that type expression is valid and return its type *)
  fun checkType te ttable =
    case te of
      Cat.Int _ => Int
    | Cat.Bool _ => Bool
    | Cat.TyVar (s,_) => TyVar s 

  fun convertType (Cat.Int _) = Int
    | convertType (Cat.Bool _) = Bool
    | convertType (Cat.TyVar (s, _)) = TyVar s

  (* Check pattern and return vtable *)
  fun checkPat pat ty (ttable : TTable) pos =
    case (pat,ty) of
      (Cat.NumP _, Int) => []
    | (Cat.VarP (x,p), ty) => [(x,ty)]
    | (Cat.TrueP pos, Bool) => []
    | (Cat.FalseP pos, Bool) => []
    | (Cat.NullP pos, TyVar _) => []
    | (Cat.TupleP (pats, pos), TyVar name) => (
      case lookup name ttable of
          SOME tys => checkPats pats tys ttable pos
        | NONE => raise Error ("wtf", pos)
      )
    | _ => raise Error ("Pattern doesn't match type", pos)

  and checkPats [] [] ttable pos = []
    | checkPats (pat::pats) (ty::tys) ttable pos = combineTables (checkPat pat ty ttable pos) (checkPats pats tys ttable pos) pos
    | checkPats _ _ _ pos = raise Error ("your mother is an error", pos)
                                  

  (* check expression and return type *)
  fun checkExp exp vtable ftable (ttable : TTable) =
    case exp of
      Cat.Num (n,pos) => Int
    | Cat.Var (x,pos) =>
       (case lookup x vtable of
	  SOME t => t
        | _ => raise Error ("Unknown variable "^x,pos))
    | Cat.Plus (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Int,Int) => Int
        | _ => raise Error ("Non-int argument to +",pos))
    | Cat.Minus (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Int,Int) => Int
        | _ => raise Error ("Non-int argument to -",pos))
    | Cat.Apply (f,e1,pos) =>
       (case lookup f ftable of
	  SOME (t1,t2) =>
	    if t1 = (checkExp e1 vtable ftable ttable)
            then t2
            else raise Error ("Argument does not match declaration of "^f,pos)
        | _ => raise Error ("Unknown function "^f,pos))
    | Cat.Read (n,pos) => Int
    | Cat.Write (e1,pos) =>
       (case checkExp e1 vtable ftable ttable of
          Int => Int
        | _ => raise Error ("Non-int argument to write",pos))

	| Cat.True pos => Bool
	| Cat.False pos => Bool
	| Cat.Equal (e1, e2, pos) =>
    	(case (checkExp e1 vtable ftable ttable,
	           checkExp e2 vtable ftable ttable) of
	       (Int,Int) => Bool
		   |(Bool,Bool) => Bool
		   |	(TyVar s1, TyVar s2) => 
					if s1 = s2 
					then Bool 
					else raise Error("Comparison of differing types ",pos)
		 | _ => raise Error("Comparison of differing types ",pos))
	| Cat.Less (e1, e2, pos) =>
    	(case (checkExp e1 vtable ftable ttable,
	           checkExp e2 vtable ftable ttable) of
	       (Int,Int) => Bool
	     | _ => raise Error ("Non-int argument to <",pos))
	| Cat.Not (e1, pos) =>
		(case (checkExp e1 vtable ftable ttable) of
	       Bool => Bool
	     | _ => raise Error ("Not has to be called with a boolean",pos))
    | Cat.And (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
            checkExp e2 vtable ftable ttable) of
        (Bool,Bool) => Bool
      | _ => raise Error ("And has to be called with two booleans",pos))
    | Cat.Or (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
            checkExp e2 vtable ftable ttable) of
        (Bool,Bool) => Bool
        | _ => raise Error ("Or has to be called with two booleans",pos))
    | Cat.If (ec, e1, e2, pos) =>
       let val (ct, t1, t2) = (
			checkExp ec vtable ftable ttable,
			checkExp e1 vtable ftable ttable, 
			checkExp e2 vtable ftable ttable)
	   in
			if t1 = t2 andalso ct = Bool
			then t1
			else raise Error ("Or has to be called with two booleans",pos)
	   end
	| Cat.Let(d, e, _) =>
    let
        val vtable' = checkDec d vtable ftable ttable
    in
        checkExp e vtable' ftable ttable
    end
	| Cat.MkTuple (exps, typname, pos) =>
		let
			fun x(exp::exps, (typ::typs) : CType list, pos, vtable, ftable, ttable) = 
				let
					val exptyp : CType = checkExp exp vtable ftable ttable
				in
					if exptyp = typ
				  	then x(exps, typs, pos, vtable, ftable, ttable)
				  	else raise Error ("Tuple of wrong type",pos)
				end
			  | x([], [], pos, _, _, _) = ()
			  | x(_,   _, pos, _, _, _) = raise Error ("Tuple of wrong type",pos)
		in
			(case lookup typname ttable of
			  SOME (typs : CType list) => (x(exps, typs, pos, vtable, ftable, ttable); TyVar (typname))
		    |       _ => raise Error ("Unknown type "^typname,pos))
		end
	| Cat.Case(e, m, pos) => 
		let
			val t = checkExp e vtable ftable ttable
			fun patCheck (t, (mpat, mexp)::ms, vtable, ftable, ttable : TTable, pos) = 
				let
					val lastexp = patCheck(t,ms,vtable,ftable,ttable, pos)
					val expt = checkExp mexp vtable ftable ttable
				in
					if lastexp <> expt
					then raise Error("Pattern mismatch", pos)
					else (checkPat mpat t ttable pos; expt)
				end
			  | patCheck(t, [], _, _, _, _) = t
		in
			patCheck( t, m, vtable, ftable, ttable, pos)
		end
	| Cat.Null (name, pos) => TyVar name

  and checkDec [] vtable ftable ttable = vtable
    | checkDec ((pat, exp, pos)::xs) vtable ftable ttable =
      let
          val expT = checkExp exp vtable ftable ttable
          val vtable' = checkPat pat expT ttable pos
      in
          checkDec xs (combineTables vtable' vtable pos) ftable ttable
      end
                 
  and checkMatch [(p,e)] tce vtable ftable ttable pos =
        let
          val vtable1 = checkPat p tce ttable pos
        in
	  checkExp e (vtable1 @ vtable) ftable ttable
        end
    | checkMatch ((p,e)::ms) tce vtable ftable ttable pos =
        let
          val vtable1 = checkPat p tce ttable pos
          val te = checkExp e (vtable1 @ vtable) ftable ttable
          val tm = checkMatch ms tce vtable ftable ttable pos
        in
	  if te = tm then te
	  else raise Error ("Match branches have different type",pos)
        end
    | checkMatch [] tce vtable ftable ttable pos =
        raise Error ("Empty match",pos)

  fun getFunDecs [] ttable ftable = ftable
    | getFunDecs ((f, targ, tresult, m, pos)::fs) ttable ftable =
        if List.exists (fn (g,_)=>f=g) ftable
	then raise Error ("Duplicate declaration of function "^f,pos)
        else getFunDecs fs ttable
			((f, (checkType targ ttable, checkType tresult ttable))
			 :: ftable)

  fun checkFunDec ftable ttable (f, targ, tresult, m, pos) =
    let
      val argtype = checkType targ ttable
      val resulttype = checkType tresult ttable
      val bodytype = checkMatch m argtype [] ftable ttable pos
    in
      if resulttype = bodytype
      then resulttype
      else raise Error ("Body type doesn't match declaration",pos)
    end

  fun checkProgram (tyDecs, funDecs, e) =
    let
      val ttable = checkDups (map (fn (s, ts, _) => (s, map convertType ts)) tyDecs)
      val ftable = getFunDecs funDecs ttable []
      val _ = List.map (checkFunDec ftable ttable) funDecs
    in
      (checkExp e [] ftable ttable; ())
    end
end
