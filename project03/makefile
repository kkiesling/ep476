SHELL = /bin/sh
FCOMP = gfortran
FLDR = gfortran

FFLAGS = -O3
FFLAGS_DBG = -g -fbounds-check
LDRFLAGS = 


LIBDIR = /usr/lib
LIBS1 = lapack
LIBS2 = blas
LIBDIR3 = $(HOME)/ep476/project03/
LIBS3 = matrixgen

OBJS = M_gen.o K_gen.o
DRIVERS = driver

# This is a module-list macro.

MODS = kind_mod.mod

all : clean driver

driver : $(MODS) $(OBJS) library
	@echo "Creating "$@" in directory "$(PWD)"."
	$(FLDR) $(FFLAGS_DBG) -o $@ $(DRIVERS).f90 $(OBJS) -L$(LIBDIR3) -l$(LIBS3) -L$(LIBDIR) -l$(LIBS1) -l$(LIBS2)


library : $(OBJS)
	ar -r lib$(LIBS3).a $(OBJS)
	ranlib lib$(LIBS3).a

# aph_dbg : FFLAGS = $(FFLAGS_DBG)
# cheb_eval :  $(OBJS)
#	@echo "Creating "$@" in directory "$(PWD)"."
#	$(FLDR) -o $@ $(LDRFLAGS) $(OBJS) $(LIBDIR) $(LIBS)

$(OBJS) : %.o : %.f90
	$(FCOMP) $(FFLAGS_DBG) -c $<

$(MODS) : %.mod : %.f90
	$(FCOMP) $(FFLAGS_DBG) -c $<

clean:
	-rm *.o
	@echo "Cleaned object and mod files from "$(PWD)"."

realclean: clean
	-rm chdriver
	@echo "Cleaned executables from "$(PWD)"."

