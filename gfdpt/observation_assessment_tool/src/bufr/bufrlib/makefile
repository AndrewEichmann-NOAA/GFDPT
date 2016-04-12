#==============================================================================
#
# Makefile for DAT library (based on MIRS)
#
# Kevin Garrett @ RTi/NOAA/NESDIS/STAR/JCDSA
#==============================================================================

#---Paths are read from a common file (used across the MIRS package)
lib_path  = ./
CRTM_path = ../../crtm/REL-2.1.3

#---Default macros and rules (from CRTM)
include $(CRTM_path)/make.macros

# -------------
# This makefile
# -------------
MAKEFILE = makefile


# ------------
# Object files
# ------------
INCLUDES  = \
	-I$(lib_path) 

LIBRARIES = 

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)


# ---------------------
# Make based on OS type
# ---------------------

objects_c := $(patsubst %.c,%.o,$(wildcard *.c))
objects_f := $(patsubst %.f,%.o,$(wildcard *.f))
objects_F := $(patsubst %.F,%.o,$(wildcard *.F))

all: $(objects_f) $(objects_F) $(objects_c)

CC = icc


libbufr.a : $(objects_f) $(objects_F) $(objects_c)


# rules for c files in BUFR 
.SUFFIXES: .c .f .o
.c.o: 
	$(CC) -c -DUNDERSCORE $< 
	ar cru libbufr.a *.o
.f.o: 
	$(FC) -c  $< 
	ar cru libbufr.a *.o

.F.o: 
	$(FC) -c  $< 
	ar cru libbufr.a *.o


#----------------
# Execution
#----------------



# --------
# Clean up
# --------
clean:
	$(REMOVE)  \
	$(OBJ_FILES) *.mod *.o *.a *.MOD *.stb *~   $(lib_path)/*.mod  $(lib_path)/*~ 




