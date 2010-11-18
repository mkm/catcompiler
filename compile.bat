mosmlc -c Mips.sml
mosmlc -c RegAlloc.sig RegAlloc.sml
mosmlc -c Cat.sml
mosmlyac -v Parser.grm
mosmlc -c Parser.sig Parser.sml
mosmllex Lexer.lex
mosmlc -c Lexer.sml
mosmlc -c Type.sig Type.sml
mosmlc -c Compiler.sig Compiler.sml
mosmlc -o CC.exe CC.sml
