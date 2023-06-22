PURPOSE = OPTIMIZE
FFLAGS = $(FORTRAN_$(PURPOSE))
CFLAGS = $(C_$(PURPOSE))

include ../Config/Makefile_Build

all: $(OBJECTS)
	ar r libpspfft.a $(OBJECTS)
	ranlib libpspfft.a
	
install:
	mkdir -p $(INSTALL)/lib
	mkdir -p $(INSTALL)/include
	find . -iname "*.mod" -exec cp '{}' $(INSTALL)/include \;
	cp libpspfft.a $(INSTALL)/lib

clean:
	rm -f *.o *.mod *MOD *.ld *.a
	
debug:
	echo $(VPATH)

include $(PATH_SOURCES)/Makefile_Dependencies
