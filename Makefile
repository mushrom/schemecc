TESTSRC  = $(wildcard tests/*.scm)
TESTOBJ  = $(TESTSRC:.scm=.o)
TESTPROG = $(TESTSRC:.scm=.progout)
TESTOUT  = $(TESTSRC:.scm=.testout)
DIFFOUT  = $(TESTSRC:.scm=.diff)

all: stub.o $(TESTPROG)

%.asm: %.scm
	@echo "SCHEMECC $< > $@"
	@gojira foo.scm $< > $@

%.o: %.asm
	@echo AS $< -o $@
	@nasm -f elf64 -o $@ $<

%.progout: %.o
	@echo CC $< -o $@
	@$(CC) $(CFLAGS) -o $@ stub.o $<

%.testout: %.progout
	@-./$< 2>&1 > $@

%.diff: %.testout
	@-diff $*.expected $< > $@

stub.o: stub.c

$(TESTPROG): stub.o foo.scm

clean-tests:
	@-echo "  > cleaning test output"
	@rm -f tests/*.o
	@rm -f tests/*.asm
	@rm -f tests/*.progout
	@rm -f tests/*.testout
	@rm -f tests/*.diff

clean-obj:
	@-echo "  > cleaning objects"
	@rm -f *.o

clean: clean-tests clean-obj

testgen: stub.o $(DIFFOUT)
	@echo "  > test output:"
	@for test in tests/*.diff; do echo "<=> $$test"; cat $$test | sed 's/.*/    | &/g'; done

test: testgen clean-tests
