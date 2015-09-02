DC=dmd
PREFIX=
TESTDIR=test
SRCDIR=src
BINDIR=bin
DFLAGS=-I$(SRCDIR)
SRC=simplerepl.d simpledatatypes.d simpleevaluate.d simpleread.d \
    simpleext01.d
OBJ=simplerepl.obj simpledatatypes.obj simpleevaluate.obj simpleread.obj \
    simpleext01.obj

all: $(OBJ)
	$(DC) -of$(BINDIR)/repl $**

simplerepl.obj: $(SRCDIR)/simplerepl.d
	$(DC) -c $(DFLAGS) $**

simpledatatypes.obj: $(SRCDIR)/simpledatatypes.d
	$(DC) -c $(DFLAGS) $**

simpleevaluate.obj: $(SRCDIR)/simpleevaluate.d
	$(DC) -c $(DFLAGS) $**

simpleread.obj: $(SRCDIR)/simpleread.d
	$(DC) -c $(DFLAGS) $**

simpleext01.obj: $(SRCDIR)/simpleext01.d
	$(DC) -c $(DFLAGS) $**

test:
	rdmd -Isrc test/simpletest.d

run:
	rdmd -Isrc src/simplerepl.d
