CC=chicken-csc
CFLAGS=

SDIR=src
ODIR=src/obj

_OBJ = core.o render.o tex.o dynamic-vec.o filesys.o input.o taiko.o font.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

OUTPUT = taiko

$(ODIR)/%.o: $(SDIR)/%.scm
	$(CC) $(CFLAGS) -c -output-file $@ $<
		
debug: $(OBJ)
	$(CC) $^ -output-file $(OUTPUT) $(CFLAGS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o *~ core
