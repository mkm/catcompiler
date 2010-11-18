load "Lexing";
load "Nonstdio";
load "Parser";
load "Lexer";

fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n);

fun showsyntax f =
  let
    val lexbuf = createLexerStream (BasicIO.open_in f)
  in
    Parser.Prog Lexer.Token lexbuf
  end
