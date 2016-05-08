TESTSRC  = $(wildcard tests/*.scm)
TESTPROG = $(TESTSRC:.scm=.progout)
TESTOUT  = $(TESTSRC:.scm=.testout)
DIFFOUT  = $(TESTSRC:.scm=.diff)
LIBSRC   = $(wildcard libs/*.scm)
LIBOBJ   = $(LIBSRC:.scm=.lib.checkpoint)

.PHONY: all
all: stub.o $(TESTPROG) $(LIBOBJ)

%_lib.asm: %.scm
	@echo "SCHEMECC -library $< > $@"
	@gojira foo.scm -library $< > $@

%.lib.checkpoint: %.scm
	@echo SCHEMECC $<
	@./foo.scm $<
	@touch $@

%.progout: %.scm
	@echo SCHEMECC $< -o $@
	@./foo.scm $<
	@mv $* $@

%.testout: %.progout
	@-./$< 2>&1 > $@

%.diff: %.testout
	@-diff $*.expected $< > $@

stub.o: stub.c

$(TESTPROG): stub.o foo.scm $(LIBOBJ)

.PHONY: clean-tests
clean-tests:
	@-echo "  > cleaning test output"
	@rm -f tests/*.o
	@rm -f tests/*.asm
	@rm -f tests/*.progout
	@rm -f tests/*.testout
	@rm -f tests/*.diff

.PHONY: clean-obj
clean-obj:
	@-echo "  > cleaning objects"
	@rm -f *.o

.PHONY: clean-libs
clean-libs:
	@-echo "  > cleaning libraries"
	@rm -f libs/*.o
	@rm -f libs/*.asm
	@rm -f libs/*.lib.checkpoint

.PHONY: clean
clean: clean-tests clean-obj clean-libs

.PHONY: testgen
testgen: stub.o $(DIFFOUT)
	@echo "  > test output:"
	@for test in tests/*.diff; do echo "<=> $$test"; cat $$test | sed 's/.*/    | &/g'; done

.PHONY: test
test: testgen clean-tests

.PHONY: libs
libs: $(LIBOBJ)
