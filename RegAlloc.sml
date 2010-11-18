structure RegAlloc :> RegAlloc =
struct

open Mips;

exception error of string

exception not_colourable

(* Funktioner, der implementerer mængder af strings *)

val emptyset = []

fun addset (a:string) [] = [a]
  | addset a (b :: l) =
      if a<b then a :: b :: l
      else if a=b then b :: l
      else b :: addset a l

fun addsetL [] s = s
  | addsetL (a :: l) s = addset a (addsetL l s)

fun union [] l2 = l2
  | union (a::l1) l2 = addset a (union l1 l2)

fun remove (a:string) [] = []
  | remove a (b :: l)  =
      if a<b then b :: l
      else if a=b then l
      else b :: remove a l

fun setdiff l1 [] = l1
  | setdiff l1 (a::l2) = remove a (setdiff l1 l2)

fun member (a:string) [] = false
  | member a (b :: l)  =
      if a<b then false
      else if a=b then true
      else member a l

fun intersect [] l2 = []
  | intersect (a::l1) l2 =
      if member a l2 then a :: intersect l1 l2
      else intersect l1 l2

fun zip [] _ = []
  | zip _ [] = []
  | zip (a :: aas) (b :: bs) = (a,b) :: zip aas bs

fun destRegs [] = []
  | destRegs (i::ilist) = union (destReg i) (destRegs ilist)

and destReg i =
  case i of
    LA (rt,v) => addset rt emptyset
  | LUI (rt,v) => addset rt emptyset
  | ADD (rd,rs,rt) => addset rd emptyset
  | ADDI (rd,rs,v) => addset rd emptyset
  | SUB (rd,rs,rt) => addset rd emptyset
  | AND (rd,rs,rt) => addset rd emptyset
  | ANDI (rd,rs,v) => addset rd emptyset
  | OR (rd,rs,rt) => addset rd emptyset
  | ORI (rd,rs,v) => addset rd emptyset
  | XOR (rd,rs,rt) => addset rd emptyset
  | XORI (rd,rs,v) => addset rd emptyset
  | SLL (rd,rt,v) => addset rd emptyset
  | SRA (rd,rt,v) => addset rd emptyset
  | SLT (rd,rs,rt) => addset rd emptyset
  | SLTI (rd,rs,v) => addset rd emptyset
  | JAL (lab,argRegs) => argRegs
  | LW (rd,rs,v) => addset rd emptyset
  | LB (rd,rs,v) => addset rd emptyset
  | SYSCALL => addset "2" emptyset (* returværdi er i $2 *)
  | _ => emptyset

fun live_step ilist llist liveAtEnd =
  let fun scan [] = []
        | scan (i::is) =
	    let val ls1 = scan is
	    in if null ls1 then [instruct i liveAtEnd]
	       else instruct i (hd ls1) :: ls1
	    end

      and instruct i live =
	    case i of	      
              LA (rt,v) => remove rt live
            | LUI (rt,v) => remove rt live
            | ADD (rd,rs,rt) => addsetL [rs,rt] (remove rd live)
            | ADDI (rd,rs,v) => addset rs (remove rd live)
            | SUB (rd,rs,rt) => addsetL [rs,rt] (remove rd live)
            | AND (rd,rs,rt) => addsetL [rs,rt] (remove rd live)
            | ANDI (rd,rs,v) => addset rs (remove rd live)
            | OR (rd,rs,rt) => addsetL [rs,rt] (remove rd live)
            | ORI (rd,rs,v) => addset rs (remove rd live)
            | XOR (rd,rs,rt) => addsetL [rs,rt] (remove rd live)
            | XORI (rd,rs,v) => addset rs (remove rd live)
            | SLL (rd,rt,v) => addset rt (remove rd live)
            | SRA (rd,rt,v) => addset rt (remove rd live)
            | SLT (rd,rs,rt) => addsetL [rs,rt] (remove rd live)
            | SLTI (rd,rs,v) => addset rs (remove rd live)
            | BEQ (rs,rt,v) => addsetL [rs,rt] (union live (live_at v))
            | BNE (rs,rt,v) => addsetL [rs,rt] (union live (live_at v))
            | J lab => live_at lab
            | JAL (lab,argRegs) => union argRegs live
		(* argRegs er argumentregistre *)
            | JR (r,resRegs) => addsetL (r::resRegs) emptyset
		(* Antag JR kun bruges som return *)
		(* resRegs er resultatregistre *)
            | LW (rd,rs,v) => addset rs (remove rd live)
            | SW (rd,rs,v) => addsetL [rs,rd] live
            | LB (rd,rs,v) => addset rs (remove rd live)
            | SB (rd,rs,v) => addsetL [rs,rd] live
            | SYSCALL => addsetL ["2","4","5"] live
                (* $2 er control register og $4 og $5 er argumenter *)
            | _ => live

      and live_at lab = search ilist llist lab

      and search [] [] lab = emptyset
        | search (LABEL k :: is) (l :: ls) lab =
	    if k = lab then l else search is ls lab
        | search (_ :: is) (_ :: ls) lab = search is ls lab
	| search a b l = search a b l (* shouldn't happen *)
  in
    scan ilist
  end

fun iterate_live ilist llist liveAtEnd =
  let val llist1 = live_step ilist llist liveAtEnd
  in if llist1 = llist then llist
     else iterate_live ilist llist1 liveAtEnd
  end

fun init_list [] = []
  | init_list (i::is) = emptyset :: init_list is

(* live_regs finder for hver instruktion de symbolske registernavne, *)
(* der er levende FØR denne instruktion er udført.                   *)

fun live_regs ilist liveAtEnd =
      iterate_live ilist (init_list ilist) liveAtEnd

fun regs [] rs = rs
  | regs (l :: llist) rs = union l (regs llist rs)

fun findRegs llist = filterSymbolic (regs llist [])

and filterSymbolic [] = []
  | filterSymbolic (a::l) =
      if numerical a then filterSymbolic l
      else a :: filterSymbolic l

(* conflicts llist r finder de registre, *)
(* der interfererer med r                *)

fun conflicts [] [] callerSaves live1 r =
      let val con1 = if numerical r then remove r callerSaves else []
          val con2 = if member r live1 then remove r live1 else []
      in
        union con1 con2
      end
  | conflicts (ORI (rd,rs,"0") :: ilist) (l :: llist) callerSaves live1 r =
      if r=rd
      then union (remove rs (remove r l))
		 (conflicts ilist llist callerSaves live1 r)
      else if r=rs
      then conflicts ilist llist callerSaves live1 r
      else if member r l
      then union [rd] (conflicts ilist llist callerSaves live1 r)
      else conflicts ilist llist callerSaves live1 r
  | conflicts (JAL (f,argRegs) :: ilist) (l :: llist) callerSaves live1 r =
      if (member r l)  (* Function calls interfere with caller-saves *)
      then union (remove r callerSaves)
		 (conflicts ilist llist callerSaves live1 r)
      else if member r callerSaves
      then union (remove r l)
		 (conflicts ilist llist callerSaves live1 r)
      else conflicts ilist llist callerSaves live1 r
  | conflicts (i :: ilist) (l :: llist) callerSaves live1 r =
      if (member r (destReg i))
      then union (remove r l)
		 (conflicts ilist llist callerSaves live1 r)
      else if member r l
      then union (destReg i)
		 (conflicts ilist llist callerSaves live1 r)
      else conflicts ilist llist callerSaves live1 r
  | conflicts _ _ _ _ _ = raise error "conflicts used at undefined instance"

(* En graf repræsenteres som en liste af registre *)
(* parret med deres konflikter                    *)

fun graph ilist llist callerSaves =
  let val rs = callerSaves @ findRegs llist
  in
    zip rs (map (conflicts ilist ((tl llist)@[[]]) callerSaves (hd llist)) rs)
  end

(* finder move-relaterede registre *)

fun findMoves ilist llist =
  let val rs = findRegs llist
  in
    zip rs (map (findMoves1 ilist) rs)
  end

and findMoves1 [] r = []
  | findMoves1 (ORI (rd,rs,"0") :: ilist) r =
      union (if rd=r then [rs]
             else if rs=r then [rd]
             else [])
	    (findMoves1 ilist r)
  | findMoves1 (i :: ilist) r = findMoves1 ilist r

(* sorter efter antallet af konflikter,
   dog med numeriske knuder til sidst   *)

fun sortByOrder [] = []
  | sortByOrder (g : (string * 'b list) list) =
     let fun split [] = ([],[])
	   | split (a::g) =
               let val (l,g1) = ascending a g []
                   val (g2,g3) = split g1
               in (rev2 l g3, g2) end
         and ascending a [] l = (a::l,[])
           | ascending a (g as (b::g1)) l =
               if numerical (#1 b)
	          orelse not (numerical (#1 a))
		  andalso length (#2 a) <= length (#2 b)
               then ascending b g1 (a::l)
               else (a::l,g)
         and rev2 [] l2 = l2
           | rev2 (a::l1) l2 = rev2 l1 (a::l2)
         fun merge [] l2 = l2
           | merge l1 [] = l1
           | merge (l1 as (a::r1)) (l2 as (b::r2)) =
               if numerical (#1 b)
		  orelse not (numerical (#1 a))
		  andalso length (#2 a) <= length (#2 b)
               then a :: merge r1 l2
               else b :: merge l1 r2
         val (g1,g2) = split g
     in 
       if null g1 then g2
       else if null g2 then g1
       else merge (sortByOrder g1) (sortByOrder g2)
     end

(* n-farv grafen ved brug af Briggs algoritme *)

fun colourGraph g rmin rmax moveRelated =
  select (simplify (sortByOrder g) []) (mList rmin rmax) moveRelated []

and simplify [] l = l
  | simplify ((a as (r,c)) :: g) l =
      simplify (sortByOrder (removeNode r g)) (a::l)

and removeNode r [] = []
  | removeNode r ((r1,c)::g) =
      (r1,remove r c) :: removeNode r g

and select [] regs moveRelated sl = sl
  | select ((r,c)::l) regs moveRelated sl =
      let val rnum = 
	       if numerical r then r
               else let val possible = NotIn c sl regs
			val related = lookUp2 r moveRelated
			val related2 = map (fn r=>lookUp r sl) related
		        val mPossible = intersect possible related2
		    in
	              if null possible then raise not_colourable
		      else if null mPossible then hd possible
		      else hd mPossible
		    end
      in
        select l regs moveRelated ((r,rnum)::sl)
      end

and NotIn [] sl regs = regs
  | NotIn (r::cs) sl regs =
      NotIn cs sl (delete (lookUp r sl) regs)

and lookUp r [] = "0"
  | lookUp r ((r1,n)::sl) =
      if numerical r then r
      else if r=r1 then n else lookUp r sl

and lookUp2 r [] = []
  | lookUp2 r ((r1,ms)::sl) =
      if r=r1 then ms else lookUp2 r sl

and delete x [] = []
  | delete x (y::l) = if x=y then delete x l else y :: delete x l

and mList m n =
  if m > n then []
  else makestring m :: mList (m+1) n

fun filterNullMoves [] allocs = []
  | filterNullMoves (ORI (rd,rs,"0") :: ilist) allocs =
      let val rd1 = lookUp rd allocs
          val rs1 = lookUp rs allocs
      in
        if rd1 = rs1 orelse rd1 = "0"
	then COMMENT ("\tori\t"^rd^","^rs^",0")
	     :: filterNullMoves ilist allocs
        else ORI (rd,rs,"0") :: filterNullMoves ilist allocs
      end
  | filterNullMoves (i :: ilist) allocs =
      i :: filterNullMoves ilist allocs

and printList [] = ""
  | printList (r :: rs) = r^" "^ printList rs

fun printGraph [] = []
  | printGraph ((r,rs) :: g) =
     [COMMENT ("interferes: "^r^" with "^printList rs)]
     @ printGraph g
		  
fun registerAlloc ilist liveAtEnd rmin callerMax rmax =
  let val llist = live_regs ilist liveAtEnd
      val callerSaves = mList rmin callerMax
      val iGraph = graph ilist llist callerSaves
      val moveRelated = findMoves ilist llist
      val allocs = colourGraph iGraph rmin rmax moveRelated
      val deadRegs = setdiff (filterSymbolic (destRegs ilist))
		             (map (#1) allocs)
      val allocs1 = allocs @ (map (fn r => (r,"0")) deadRegs)
      val ilist1 = filterNullMoves ilist allocs1
      val ilist2 = List.concat (List.map (renameReg allocs1) ilist1)
  in
    (ilist2, hd llist, maxreg allocs 0)
  end

and maxreg [] m = m
  | maxreg ((r,n)::rs) m = 
      maxreg rs (if m< intOfString n then intOfString n else m)

and renameReg allocs inst =
    case inst of
      LA (rt,v) =>
        [LA (lookUp rt allocs, v),
         COMMENT ("was:\tla\t"^rt^", "^v)]
    | LUI (rt,v) =>
        [LUI (lookUp rt allocs, v),
         COMMENT ("was:\tlui\t"^rt^", "^v)]
    | ADD (rd,rs,rt) =>
        [ADD (lookUp rd allocs, lookUp rs allocs, lookUp rt allocs),
         COMMENT ("was:\tadd\t"^rd^", "^rs^", "^rt)]
    | ADDI (rd,rs,v) =>
        [ADDI (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\taddi\t"^rd^", "^rs^", "^v)]
    | SUB (rd,rs,rt) =>
        [SUB (lookUp rd allocs, lookUp rs allocs, lookUp rt allocs),
         COMMENT ("was:\tsub\t"^rd^", "^rs^", "^rt)]
    | AND (rd,rs,rt) =>
        [AND (lookUp rd allocs, lookUp rs allocs, lookUp rt allocs),
         COMMENT ("was:\tand\t"^rd^", "^rs^", "^rt)]
    | ANDI (rd,rs,v) =>
        [ANDI (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tandi\t"^rd^", "^rs^", "^v)]
    | OR (rd,rs,rt) =>
        [OR (lookUp rd allocs, lookUp rs allocs, lookUp rt allocs),
         COMMENT ("was:\tor\t"^rd^", "^rs^", "^rt)]
    | ORI (rd,rs,v) =>
        [ORI (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tori\t"^rd^", "^rs^", "^v)]
    | XOR (rd,rs,rt) =>
        [XOR (lookUp rd allocs, lookUp rs allocs, lookUp rt allocs),
         COMMENT ("was:\txor\t"^rd^", "^rs^", "^rt)]
    | XORI (rd,rs,v) =>
        [XORI (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\txori\t"^rd^", "^rs^", "^v)]
    | SLL (rd,rt,v) =>
        [SLL (lookUp rd allocs, lookUp rt allocs, v),
         COMMENT ("was:\tsll\t"^rd^", "^rt^", "^v)]
    | SRA (rd,rt,v) =>
        [SRA (lookUp rd allocs, lookUp rt allocs, v),
         COMMENT ("was:\tsra\t"^rd^", "^rt^", "^v)]
    | SLT (rd,rs,rt) =>
        [SLT (lookUp rd allocs, lookUp rs allocs, lookUp rt allocs),
         COMMENT ("was:\tslt\t"^rd^", "^rs^", "^rt)]
    | SLTI (rd,rs,v) =>
        [SLTI (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tandi\t"^rd^", "^rs^", "^v)]
    | BEQ (rs,rt,v) =>
        [BEQ (lookUp rs allocs, lookUp rt allocs, v),
         COMMENT ("was:\tbeq\t"^rs^", "^rt^", "^v)]
    | BNE (rs,rt,v) =>
        [BNE (lookUp rs allocs, lookUp rt allocs, v),
         COMMENT ("was:\tbne\t"^rs^", "^rt^", "^v)]
    | JAL (lab,argRegs) =>
        [JAL (lab, List.map (fn r=>lookUp r allocs) argRegs),
	 COMMENT ("was:\tjal\t"^lab^", "^String.concat argRegs)]
    | JR (r, resRegs) =>
        [JR (lookUp r allocs, List.map (fn r=>lookUp r allocs) resRegs),
	 COMMENT ("was:\tjr\t"^r^", "^String.concat resRegs)]
    | LW (rd,rs,v) =>
        [LW (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tlw\t"^rd^", "^v^"("^rs^")")]
    | SW (rd,rs,v) =>
        [SW (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tsw\t"^rd^", "^v^"("^rs^")")]
    | LB (rd,rs,v) =>
        [LB (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tlb\t"^rd^", "^v^"("^rs^")")]
    | SB (rd,rs,v) =>
        [SB (lookUp rd allocs, lookUp rs allocs, v),
         COMMENT ("was:\tsb\t"^rd^", "^v^"("^rs^")")]
    | _ => [inst]
 

end
