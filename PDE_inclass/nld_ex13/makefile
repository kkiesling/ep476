#-----------------------------------------------------------------------
# This is the makefile for the nldiff program.  
# Note that lines such as these that begin with the
# 'pound' symbol are comment lines.
#
# Possible targets for this descriptor file are nld_exec (the
# default optimized executable), nld_dbg (a debugging version),
# clean (to remove *.o and *.mod files), and realclean to also
# remove the executables.
#-----------------------------------------------------------------------

# The following are considered macros.  We'll use them
# to specify a shell and the compiler and loader programs.

SHELL = /bin/sh
FCOMP = gfortran -fconvert='big-endian'
FLDR = gfortran -fconvert='big-endian'

# ar and ranlib commands

AR = ar
RANLIB = ranlib

# Compiler and loader-option flags are typically listed in
# separate macros.  The macro with -O3 specifies optimization,
# while the macro with -g -C is used for debugging and
# runtime checks of array bounds.


FFLAGS_OPT = -O3
FFLAGS_DBG = -g -fbounds-check

LDRFLAGS = 

# Library paths and libraries can be listed here.

LIBDIR1 = $(HOME)/ep476/PDE_inclass/nld_ex13 
LIBS1 = nld
LIBDIR2 = /usr/lib
LIBS2 = lapack
LIBS3 = blas

#-----------------------------------------------------------------------

# The following macro lists all of the object files that are
# needed to build the executable.  The "\" signifies
# that the line is continued.

OBJS = nld_kind_mod.o nld_inp_mod.o nld_io_mod.o nld_data_mod.o \
       nld_coeff.o nld_expladv.o nld_time.o nldiff.o nld_impladv.o \
       nld_interp.o

MODS = nld_kind_mod.mod nld_inp_mod.mod nld_io_mod.mod nld_data_mod.mod \
       nld_impladv.mod nld_interp.mod

#-----------------------------------------------------------------------

# The first dependency list is the default, so it should be
# used for the final executable.  Other dependencies may get
# invoked in the process of creating the final executable.
# Note that $(macro) evaluates a macro or an environment variable,
# and the list of commands or rules follow the dependency.

nld_exec : FFLAGS = $(FFLAGS_DBG)
nld_exec : library
	@echo "Creating "$@" in directory "$(PWD)"."
	$(FLDR) -o $@ $(LDRFLAGS) $(OBJS) -L$(LIBDIR1) -l$(LIBS1) -L$(LIBDIR2) -l$(LIBS2) -l$(LIBS3)

# The following dependency is similar, but it conditionally
# replaces the value of FFLAGS with FFLAGS_DBG when
# processing the rules for the target nld_dbg.

nld_dbg : FFLAGS = $(FFLAGS_DBG)
nld_dbg : $(OBJS)
	@echo "Creating "$@" in directory "$(PWD)"."
	$(FLDR) -o $@ $(LDRFLAGS) $(OBJS) $(LIBDIR) $(LIBS)

library : $(MODS) $(OBJS)
	ar -r lib$(LIBS1).a $(OBJS)
	ranlib lib$(LIBS1).a

#-----------------------------------------------------------------------

# The following are static pattern rules that provide
# instructions for creating object files from the fortran
# source files.

$(OBJS) : %.o : %.f
	$(FCOMP) $(FFLAGS) -c $<

$(MODS) : %.mod : %.f
	$(FCOMP) $(FFLAGS) -c $<

# We also need to list the module dependencies.

nld_io_mod.o: nld_kind_mod.mod
nld_inp_mod.o: nld_kind_mod.mod
nld_data_mod.o: nld_kind_mod.mod nld_inp_mod.mod
nld_coeff.o: nld_kind_mod.mod nld_inp_mod.mod nld_data_mod.mod
nld_expladv.o: nld_kind_mod.mod nld_inp_mod.mod nld_data_mod.mod
nld_impladv.o: nld_kind_mod.mod nld_inp_mod.mod nld_data_mod.mod
nld_time.o: nld_kind_mod.mod
nldiff.o: nld_kind_mod.mod nld_io_mod.mod nld_inp_mod.mod \
          nld_data_mod.mod

#-----------------------------------------------------------------------

# Specifying 'clean' dependencies is also handy.  With no
# listed dependencies, clean is considered a 'phony' target.
# The dash in front of a command means to continue even if that
# command returns a nonzero error code.

clean:
	-rm *.o *.mod
	@echo "Cleaned object and mod files from "$(PWD)"."

realclean: clean
	-rm nld_exec nld_dbg
	@echo "Cleaned executables from "$(PWD)"."

