(* Compiler for Cat *)
(* Compile by mosmlc -c Compiler.sml *)
(* Then recompile CC by mosmlc CC.sml -o CC *)

structure Compiler :> Compiler =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  (* Name generator.  Call with, e.g., t1 = "tmp"^newName () *)
  val counter = ref 0

  fun newName () = (counter := !counter + 1;
                  "_" ^ Int.toString (!counter)^ "_")

  (* Number to text with spim-compatible sign symbol *)
  fun makeConst n = if n>=0 then Int.toString n
                    else "-" ^ Int.toString (~n)

  fun lookup x [] pos = raise Error ("Name "^x^" not found", pos)
    | lookup x ((y,v)::table) pos = if x=y then v else lookup x table pos

  fun isIn x [] = false
    | isIn x (y::ys) = x=y orelse isIn x ys

  (* link register *)
  val RA = "31"
  (* Register for stack pointer *)
  val SP = "29"
  (* Register for heap pointer *)
  val HP = "28"

  (* Suggested register division *)
  val maxCaller = 15   (* highest caller-saves register *)
  val maxReg = 26      (* highest allocatable register *)

  (* compile pattern *)
  fun compilePat p v vtable fail =
    case p of
      Cat.NumP (n,pos) =>
        let
	  val t = "_constPat_"^newName()
        in
          if n<32768 then
	    ([Mips.LI (t, makeConst n),
	      Mips.BNE (v,t,fail)],
	     vtable)
	  else
	    ([Mips.LUI (t, makeConst (n div 65536)),
	      Mips.ORI (t, t, makeConst (n mod 65536)),
	      Mips.BNE (v,t,fail)],
	     vtable)
	end
    | Cat.VarP (x,pos) =>
        let
          val xt = "_patVar_"^x^"_"^newName()
        in
          ([Mips.MOVE (xt,v)], (x,xt)::vtable)
        end

  (* compile expression *)
  fun compileExp e vtable place =
    case e of
      Cat.Num (n,pos) =>
        if n<32768 then
	  [Mips.LI (place, makeConst n)]
	else
	  [Mips.LUI (place, makeConst (n div 65536)),
	   Mips.ORI (place, place, makeConst (n mod 65536))]
    | Cat.Var (x,pos) => [Mips.MOVE (place, lookup x vtable pos)]
    | Cat.Plus (e1,e2,pos) =>
        let
	  val t1 = "_plus1_"^newName()
	  val t2 = "_plus2_"^newName()
          val code1 = compileExp e1 vtable t1
          val code2 = compileExp e2 vtable t2
	in
	  code1 @ code2 @ [Mips.ADD (place,t1,t2)]
	end
    | Cat.Minus (e1,e2,pos) =>
        let
	  val t1 = "_minus1_"^newName()
	  val t2 = "_minus2_"^newName()
          val code1 = compileExp e1 vtable t1
          val code2 = compileExp e2 vtable t2
	in
	  code1 @ code2 @ [Mips.SUB (place,t1,t2)]
	end
    | Cat.Apply (f,e,pos) =>
	let
	  val t1 = "_apply_"^newName()
	  val code1 = compileExp e vtable t1
	in
	  code1 @
          [Mips.MOVE ("2",t1), Mips.JAL (f,["2"]), Mips.MOVE (place,"2")]
	end
    | Cat.Read pos =>
        [Mips.LI ("2","5"), (* read_int syscall *)
         Mips.SYSCALL,
         Mips.MOVE (place,"2")]
    | Cat.Write (e,pos) =>
	compileExp e vtable place
        @ [Mips.MOVE ("4",place),
	   Mips.LI ("2","1"),  (* write_int syscall *)
	   Mips.SYSCALL,
	   Mips.LA ("4","_cr_"),
	   Mips.LI ("2","4"),  (* write_string syscall *)
	   Mips.SYSCALL]

  and compileMatch [] arg res endLabel failLabel vtable =
        [Mips.J failLabel]
    | compileMatch ((p,e)::m) arg res endLabel failLabel vtable =
        let
	  val next = "_match_"^newName()
	  val (code1, vtable1) = compilePat p arg vtable next
	  val code2 = compileExp e vtable1 res
	  val code3 = compileMatch m arg res endLabel failLabel vtable
	in
	  code1 @ code2 @ [Mips.J endLabel, Mips.LABEL next] @ code3
	end

  (* code for saving and restoring callee-saves registers *)
  fun stackSave currentReg maxReg savecode restorecode offset =
    if currentReg > maxReg
    then (savecode, restorecode, offset)  (* done *)
    else stackSave (currentReg+1)
                   maxReg
                   (Mips.SW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: savecode) (* save register *)
                   (Mips.LW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: restorecode) (* restore register *)
                   (offset-4) (* adjust offset *)


  (* compile function declaration *)
  and compileFun (fname, argty, resty, m, (line,col)) =
        let
	  val atmp = fname ^"_arg_"^ newName()
          val rtmp = fname ^"_res_"^ newName()
          val exit = fname ^"_return_"^ newName()
          val fail = fname ^"_fail_"^ newName()
	  val parcode       (* move R2 to argument *)
            = [Mips.MOVE (atmp, "2")]
          val returncode    (* move return value to R2 *)
            = [Mips.LABEL exit, Mips.MOVE ("2",rtmp)]
          val errorcode     (* if match fails *)
	    = [Mips.LABEL fail,
	       Mips.LI ("5",makeConst line),
	       Mips.J "_Error_"]
          val body = compileMatch m atmp rtmp exit fail []
          val (body1, _, maxr)  (* call register allocator *)
            = RegAlloc.registerAlloc
                (parcode @ body @ returncode) ["2"] 2 maxCaller maxReg
          val (savecode, restorecode, offset) = (* save/restore callee-saves *)
                stackSave (maxCaller+1) maxr [] [] (~8)
        in
            [Mips.COMMENT "",
             Mips.LABEL fname,  (* function label *)
             Mips.SW (RA, SP, "-4")] (* save return address *)
          @ savecode  (* save callee-saves registers *)
          @ [Mips.ADDI (SP,SP,makeConst offset)] (* move SP down *)
          @ body1  (* code for function body *)
          @ [Mips.ADDI (SP,SP,makeConst (~offset))] (* move SP up *)
          @ restorecode  (* restore callee-saves registers *)
          @ [Mips.LW (RA, SP, "-4"), (* restore return addr *)
             Mips.JR (RA, [])] (* return *)
	  @ errorcode
        end


  (* compile program *)
  fun compile (tys, funs, e) =
    let
      val funsCode = List.concat (List.map compileFun funs)
      val mainCode = compileExp e [] "dead" @ [Mips.J "_stop_"]
      val (code1, _, _)
             = RegAlloc.registerAlloc mainCode [] 2 maxCaller maxReg
    in
      [Mips.TEXT "0x00400000",
       Mips.GLOBL "main",
       Mips.LABEL "main",
       Mips.LA (HP, "_heap_")]    (* initialise heap pointer *)
      @ code1                     (* run program *)
      @ funsCode		  (* code for functions *)
      @ [Mips.LABEL "_stop_",
         Mips.LI ("2","10"),      (* syscall control = 10 *)
         Mips.SYSCALL,            (* exit *)
         Mips.LABEL "_Error_",    (* code for reporting match errors *)
	 Mips.LA ("4","_ErrorString_"),
	 Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
	 Mips.MOVE ("4","5"),
	 Mips.LI ("2","1"), Mips.SYSCALL, (* print line number *)
	 Mips.LA ("4","_cr_"),
	 Mips.LI ("2","4"), Mips.SYSCALL, (* print CR *)
	 Mips.J "_stop_",
	 Mips.DATA "",
	 Mips.LABEL "_cr_",       (* carriage return string *)
	 Mips.ASCIIZ "\n",
	 Mips.LABEL "_ErrorString_",
	 Mips.ASCIIZ "Match failed near line ",
	 Mips.ALIGN "2",
	 Mips.LABEL "_heap_",
	 Mips.SPACE "100000"]
    end

end
