SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: main
main: TM.smi Eval.smi main.smi TM.o Eval.o main.o
	$(SMLSHARP) $(LDFLAGS) -o main main.smi $(LIBS)
TM.o: TM.sml TM.smi
	$(SMLSHARP) $(SMLFLAGS) -o TM.o -c TM.sml
Eval.o: Eval.sml TM.smi Eval.smi
	$(SMLSHARP) $(SMLFLAGS) -o Eval.o -c Eval.sml
main.o: main.sml TM.smi Eval.smi main.smi
	$(SMLSHARP) $(SMLFLAGS) -o main.o -c main.sml
