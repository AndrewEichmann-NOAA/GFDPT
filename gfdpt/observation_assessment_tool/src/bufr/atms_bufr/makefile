#==============================================================================
#
# Makefile for atms2fmsdr
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

#---Paths are read from a common file (used across the MIRS package)
lib_path   = ../../lib
exec_path  = ../../../bin
CRTM_path  = ../../crtm/REL-2.1.3
BUFR_path  = ../bufrlib
MAIN_path     = .
CURR_path     = `pwd`

#---Default macros and rules (from CRTM)
include $(CRTM_path)/make.macros


# -------------
# This makefile
# -------------
MAKEFILE = makefile


# ----------------
# Executable 
# ----------------
EXE_FILE_1 = $(exec_path)/atms_bufr2fmsdr

# ------------
# Object files
# ------------
OBJ_MAIN_FILES_1  =  $(MAIN_path)/atms_bufr2fmsdr.o

# ------------
# Src files
# ------------
SRC_MAIN_FILE_1  =  $(OBJ_MAIN_FILES_1:.o=.f90)


OBJ_CRTM_FILES    =
OBJ_UTILITY_FILES =  \
	$(lib_path)/misc.o                  $(lib_path)/utils.o          \
        $(lib_path)/Consts.o                $(lib_path)/IO_Scene.o       \
	$(lib_path)/IO_MeasurData.o         $(lib_path)/PreClassif.o     \
	$(lib_path)/FwdOperator.o           $(lib_path)/mathFcts.o       \
        $(lib_path)/IO_InstrConfig.o        $(lib_path)/IO_Misc_LE.o     \
	$(lib_path)/IO_Noise.o              $(lib_path)/IO_Misc.o        \
        $(lib_path)/ErrorHandling.o         $(lib_path)/atms_spatial_average_mod.o \
	$(lib_path)/Noise.o                 $(lib_path)/kinds.o

		     
OBJ_FILES_1       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_1)

SRC_UTILITY_FILES = $(OBJ_UTILITY_FILES:.o=.f90) 
SRC_CRTM_FILES    = $(OBJ_CRTM_FILES:.o=.f90)

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES = -I$(CRTM_path)/include -I$(lib_path)
LIBRARIES = -L$(CRTM_path)/lib -lCRTM -L$(BUFR_path) -lbufr

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)


#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ---------------------
# Make based on OS type
# ---------------------
all:
	@echo "OS type detected: "`uname -s` 
	@case `uname -s` in \
		"SunOS"  ) make -f $(MAKEFILE) the_program $(SUNOS_FLAGS)  ;; \
		"AIX"    ) make -f $(MAKEFILE) the_program $(AIX_FLAGS)    ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
		"Linux"  ) make -f $(MAKEFILE) the_program $(LINUX_FLAGS)  ;; \
		*        ) echo "This system is not supported"             ;; \
	esac

# ---------------------
# Make the test program
# ---------------------
the_program : $(EXE_FILE_1)

# ---------------
# Dependency list
# ---------------


$(OBJ_UTILITY_FILES) : $(SRC_UTILITY_FILES)
	cd $(lib_path) ; make

$(OBJ_MAIN_FILES_1) : $(SRC_MAIN_FILE_1) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_MAIN_FILE_1) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_1)

$(EXE_FILE_1) : $(OBJ_MAIN_FILES_1) 
	$(FL) $(OBJ_FILES_1) $(FL_FLAGS) $(EXE_FILE_1)


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(EXE_FILE_1) $(OBJ_MAIN_FILES_S) 
