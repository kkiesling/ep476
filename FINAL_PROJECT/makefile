SHELL = /bin/sh
FCOMP = gfortran
FLDR = gfortran

FFLAGS = -O3
FFLAGS_DBG = -g -fbounds-check
LDRFLAGS = 

# External library paths and libraries can be listed here.

LIBDIR = $(HOME)/ep476/FINAL_PROJECT
LIB = mc
#-----------------------------------------------------------------------

# The following macro lists all of the object files that are
# needed to build the executable.  The "\" signifies
# that the line is continued.
OBJS = knd_mod.o inp_mod.o
DRIVERS = pt_driver

# This is a module-list macro.

MODS = knd_mod.mod inp_mod.mod

#-----------------------------------------------------------------------
all : clean pt_driver

pt_driver : library
	@echo "Creating "$@" in directory "$(PWD)"."
	$(FLDR) $(FFLAGS_DBG) -o $@ $(DRIVERS).f90 $(OBJS) -L$(LIBDIR) -l$(LIB)


library : $(MODS) $(OBJS)
	ar -r lib$(LIB).a $(OBJS)
	ranlib lib$(LIB).a

# The following dependency is similar, but it conditionally
# replaces the value of FFLAGS with FFLAGS_DBG when
# processing the rules for the target aph_dbg.

# aph_dbg : FFLAGS = $(FFLAGS_DBG)
# cheb_eval :  $(OBJS)
#	@echo "Creating "$@" in directory "$(PWD)"."
#	$(FLDR) -o $@ $(LDRFLAGS) $(OBJS) $(LIBDIR) $(LIBS)

#-----------------------------------------------------------------------

# The following is a static pattern rule that provides
# instructions for creating object files from the fortran
# source files.  The older suffix rule would have been ".f.o:"

# The symbol at the end is a macro for the current source file.

$(OBJS) : %.o : %.f90
	$(FCOMP) $(FFLAGS_DBG) -c $<

$(MODS) : %.mod : %.f90
	$(FCOMP) $(FFLAGS_DBG) -c $<

#$(MODS) : %.mod : %.f90
#	$(FCOMP) $(FFLAGS) -c $<

# A list of module dependencies ensures that module information
# specific to a particular source is available.

#-----------------------------------------------------------------------

# Specifying 'clean' dependencies is also handy.  With no
# listed dependencies, clean is considered a 'phony' target.
# The dash in front of a command means to continue even if that
# command returns a nonzero error code.

clean:
	-rm *.o
	@echo "Cleaned object and mod files from "$(PWD)"."

realclean: clean
	-rm chdriver
	@echo "Cleaned executables from "$(PWD)"."

