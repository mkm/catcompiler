SMLC=/opt/mosml/bin/mosmlc
SMLLEX=/opt/mosml/bin/mosmllex
SMLYAC=/opt/mosml/bin/mosmlyac
ASM=java -jar Mars.jar

RUNTESTS=fib.test case.test
COMPTESTS=pair.type.asm ackermann.type.asm logic.type.asm option.type.asm qsort.type.asm reverse.type.asm rwlist.type.asm treesort.type.asm tuple.type.asm equal.type.asm
OBJS=Cat.uo Compiler.uo Lexer.uo Mips.uo Parser.uo RegAlloc.uo Type.uo
CC=./CC

test : testresult $(COMPTESTS)

testresult : $(RUNTESTS)
	cat $(RUNTESTS) >$@
	test -z ${cat testresult}

$(CC) : CC.sml $(OBJS)
	$(SMLC) -o $@ $<

%.uo : %.sig %.sml
	$(SMLC) -c $^

%.uo : %.sml
	$(SMLC) -c $^

%.sml : %.lex
	$(SMLLEX) $<

%.sig : %.grm
	$(SMLYAC) -v $<

%.type.asm : %.cat $(CC)
	$(CC) $* --ignore-gen-error

%.asm : %.cat $(CC)
	$(CC) $*

%.test : %.asm %.in %.out
	$(ASM) $*.asm <$*.in | diff - $*.out >$@

Parser.sig : Parser.grm

clean:
	rm -f *.uo *.ui $(CC) *.asm *.test Lexer.sml Parser.sml Parser.sig testresult

Lexer.sml : Lexer.lex
Parser.sml : Parser.sig

Compiler.uo : Mips.uo RegAlloc.uo Cat.uo Parser.uo Lexer.uo Type.uo
Type.uo : Mips.uo RegAlloc.uo Cat.uo Parser.uo Lexer.uo
Lexer.uo : Mips.uo RegAlloc.uo Cat.uo Parser.uo
Parser.uo : Mips.uo RegAlloc.uo Cat.uo
Cat.uo : Mips.uo RegAlloc.uo
RegAlloc.uo : Mips.uo
