SMLC=/opt/mosml/bin/mosmlc
SMLLEX=/opt/mosml/bin/mosmllex
SMLYAC=/opt/mosml/bin/mosmlyac
ASM=java -jar Mars.jar

TESTS=fib.test pair.test
OBJS=Cat.uo Compiler.uo Lexer.uo Mips.uo Parser.uo RegAlloc.uo Type.uo
CC=./CC

testresult : $(TESTS)
	cat $(TESTS) >$@
	test -z ${cat testresult}

$(CC) : CC.sml $(OBJS)
	$(SMLC) -o $@ $< >/dev/null

%.uo : %.sig %.sml
	$(SMLC) -c $^

%.uo : %.sml
	$(SMLC) -c $^

%.sml : %.lex
	$(SMLLEX) $<

%.sig : %.grm
	$(SMLYAC) -v $<

%.asm : %.cat $(CC)
	$(CC) $*

%.test : %.asm %.in %.out
	$(ASM) $*.asm <$*.in | diff - $*.out >$@

Parser.sig : Parser.grm

clean:
	rm -f *.uo *.ui $(CC) *.asm *.test Lexer.sml Parser.sml

Lexer.sml : Lexer.lex
Parser.sml : Parser.sig

Compiler.uo : Mips.uo RegAlloc.uo Cat.uo Parser.uo Lexer.uo Type.uo
Type.uo : Mips.uo RegAlloc.uo Cat.uo Parser.uo Lexer.uo
Lexer.uo : Mips.uo RegAlloc.uo Cat.uo Parser.uo
Parser.uo : Mips.uo RegAlloc.uo Cat.uo
Cat.uo : Mips.uo RegAlloc.uo
RegAlloc.uo : Mips.uo
