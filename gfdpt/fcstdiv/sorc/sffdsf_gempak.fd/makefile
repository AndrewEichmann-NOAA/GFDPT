SHELL=/bin/sh
#
# Invoked by the command line
#       make -f makefile
# will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#       make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#       make -f makefile clean
#
# To remove everything but the source files use the command line
#       make -f makefile clobber
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS= sffdsf.f sfdchk.f sfdinp.f sfdopn.f sfdrdd.f sfdsst.f
OBJS= sffdsf.o sfdchk.o sfdinp.o sfdopn.o sfdrdd.o sfdsst.o
# Tunable parameters
#
# FC            Name of the fortran compiling system to use
# LDFLAGS       Flags to the loader
# LIBS          List of libraries
# CMD           Name of the executable
# PROFLIB       Library needed for profiling
#
FC =           ifort 
# BSM LDFLAGS = -s -q32
LDFLAGS = -s 
LIBS =          -L/usr/lib64 -lXm -lXt -lX11 \
                -L/nwprod/gempak/nawips_intel/os/linux2.6.32_x86_64/lib -lnamsnd -lprmcnvlib -ljwblib -lsflib -lsnlib \
		 -lgemlib -lappl -lsyslib -lcgemlib -lncepBUFR -ltextlib -lxml2 -lxslt -liconv -lgemlib 
CMD =          sffdsf
PROFLIB =       -lprof

# BSM FFLAGS =  -q32\
# BSM          -bloadmap:sffdsf.cerrors -bnoquiet\
#
FFLAGS =  -I/nwprod/gempak/nawips_intel/gempak/include\
	  -I/nwprod/gempak/nawips_intel/os/linux2.6.32_x86_64/include

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:            $(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:    $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD) $(CMD).prof

