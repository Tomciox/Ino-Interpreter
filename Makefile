all:
	bnfc Ino.ebnf
	happy -gca ParIno.y
	alex -g LexIno.x
	ghc --make Interpreter.hs -o Interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocIno.* LexIno.* ParIno.* LayoutIno.* SkelIno.* PrintIno.* TestIno.* AbsIno.* TestIno ErrM.* SharedString.* ComposOp.* Ino.dtd XMLIno.* Makefile*

