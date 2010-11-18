(* Cat compiler main program *)
(* compile with mosmlc CC.sml -o CC *)
structure CC =
struct

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n");

  fun compile filename =  
      let
        val lexbuf = createLexerStream
			  (BasicIO.open_in (filename ^ ".cat"))
      in
        let
          val pgm = Parser.Prog Lexer.Token lexbuf
	  val () = Type.checkProgram pgm
          val code = Compiler.compile pgm
        in 
          let
            val outfile = TextIO.openOut (filename ^ ".asm")
          in
            (TextIO.output (outfile, Mips.pp_mips_list code);
	     TextIO.closeOut outfile)
          end
        end
          handle Parsing.yyexit ob => errorMess "Parser-exit\n"
               | Parsing.ParseError ob =>
                   let val (lin,col) = Lexer.getPos lexbuf
                   in
                     errorMess ("Parse-error at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
                   end
               | Lexer.LexicalError (mess,(lin,col)) =>
                     errorMess ("Lexical error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
	       | Compiler.Error (mess,(lin,col)) =>
                     errorMess ("Compiler error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
	       | Type.Error (mess,(lin,col)) =>
                     errorMess ("Type error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
               | SysErr (s,_) => errorMess ("Exception: " ^ s)
      end

  val _ = compile (List.nth(Mosml.argv (),1))

end
