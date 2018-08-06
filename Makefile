VPATH+=/opt/local/include
FC=gfortran
FFLAGS= -O3 -Wall -fcheck=all #-I/opt/local/lib/opt/local/bin/gfortran-mp-7/modules/
LDFLAGS= -L/opt/local/lib -lgiza

SRC=giza-fortran.F90 utils_ppm.f90 utils_colourmaps.f90 quantize.f90 extract_colourmap.f90
OBJ1=${SRC:.F90=.o}
OBJ=${OBJ1:.f90=.o}

%.o: %.F90
	$(FC) $(FFLAGS) -o $@ -c $<

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

getcmap: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LDFLAGS)

clean:
	rm *.o *.mod
