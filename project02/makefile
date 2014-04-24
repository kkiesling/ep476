#-----------------------------------------------------------------------
# This is the makefile for the aphi_comp program.  
# Note that lines such as these that begin with the
# 'pound' symbol are comment lines.
#
# Possible targets for this descriptor file are aph_exec (the
# default optimized executable), aph_dbg (a debugging version),
# clean (to remove *.o), and realclean to also remove the executables.
#-----------------------------------------------------------------------
# =================================================
# TO CHANGE: 
# replace OBJS with cheb_coll cheb_eval cheb_fit
# no MODS
# change executable names to match OBJ files
# add library paths/directories
# list of dependencies at bottom of page
# ================================================

# The following are considered macros.  We'll use them
# to specify a shell and the compiler and loader programs.

SHELL = /bin/sh
FCOMP = gfortran
FLDR = gfortran

# Compiler and loader-option flags are typically listed in
# separate macros.  The macro with -O3 specifies optimization,
# while the macro with -g -fbounds-check is used for debugging and
# runtime checks of array bounds.

FFLAGS = -O3
FFLAGS_DBG = -g -fbounds-check
LDRFLAGS = 

# External library paths and libraries can be listed here.

LIBDIR = $(HOME)/ep476/project02
LIBS = chebyshev

#-----------------------------------------------------------------------

# The following macro lists all of the object files that are
# needed to build the executable.  The "\" signifies
# that the line is continued.
OBJS = cheb_coll.o cheb_fit.o cheb_eval.o
DRIVERS = chdriver

# This is a module-list macro.

MODS = 

#-----------------------------------------------------------------------
all : clean chdriver

# The first dependency list is the default, so it should be
# used for the final executable.  Other dependencies may get
# invoked in the process of creating the final executable.
# Note that $(macro) evaluates a macro or an environment variable,
# and the list of commands or rules follow the dependency.

chdriver : library
	@echo "Creating "$@" in directory "$(PWD)"."
	$(FLDR) $(FFLAGS) -o $@ $(DRIVERS).f90 $(OBJS) -L$(LIBDIR) -l$(LIBS)


library : $(OBJS)
	ar -r lib$(LIBS).a $(OBJS)
	ranlib lib$(LIBS).a

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
	$(FCOMP) $(FFLAGS) -c $<

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

