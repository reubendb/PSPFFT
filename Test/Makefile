PURPOSE = OPTIMIZE
FFLAGS = $(FORTRAN_$(PURPOSE))
CFLAGS = $(C_$(PURPOSE))

include ../Config/Makefile_Config
include ../Config/Makefile_Rules
include IO/Makefile_ObjectFiles

VPATH = IO/

INCLUDE   = -I ../Build  $(INCLUDE_SILO) $(INCLUDE_FFTW)
LIBRARIES = -L ../Build -lpspfft $(LIBRARY_SILO) $(LIBRARY_FFTW)

all: HomogeneousSpheroid_3D

HomogeneousSpheroid_3D: \
  $(IO) HomogeneousSpheroid_3D_Program.o
	$(LINK) $(FLAGS) -o HomogeneousSpheroid_3D \
	  $(IO) HomogeneousSpheroid_3D_Program.o $(LIBRARIES)
          
clean:
	rm -f *.o *.mod *.MOD *.ld HomogeneousSpheroid_3D 

debug:
	echo $(VPATH)

HomogeneousSpheroid_3D_Program.o: \
  IO.o \
  HomogeneousSpheroid_3D_Program.f90

include IO/Makefile_Dependencies
