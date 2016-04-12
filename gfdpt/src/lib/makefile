#==============================================================================
#
# Makefile for DAT library (based on MIRS)
#
# Kevin Garrett @ RTi/NOAA/NESDIS/STAR/JCDSA
#==============================================================================

#---Paths are read from a common file (used across the MIRS package)
lib_path  = ./
CRTM_path = ../crtm/REL-2.1.3

#---Default macros and rules (from CRTM)
include $(CRTM_path)/make.macros
include $(CRTM_path)/make.rules

# -------------
# This makefile
# -------------
MAKEFILE = makefile


# ------------
# Object files
# ------------
OBJ_FILES_SUB = \
	$(lib_path)/misc.o                  $(lib_path)/utils.o          \
        $(lib_path)/Consts.o                $(lib_path)/IO_Scene.o       \
        $(lib_path)/atms_spatial_average_mod.o \
        $(lib_path)/IO_SceneAMV.o       \
        $(lib_path)/IO_DEP.o       \
        $(lib_path)/TraceGas.o       \
	$(lib_path)/IO_MeasurData.o         $(lib_path)/PreClassif.o     \
	$(lib_path)/FwdOperator.o           $(lib_path)/mathFcts.o       \
        $(lib_path)/IO_InstrConfig.o        $(lib_path)/IO_Misc_LE.o     \
	$(lib_path)/IO_Noise.o              $(lib_path)/IO_Misc.o        \
        $(lib_path)/ErrorHandling.o         $(lib_path)/CntrlParams.o    \
        $(lib_path)/TuningParams.o          $(lib_path)/GeophCovBkg.o    \
	$(lib_path)/Noise.o                 $(lib_path)/kinds.o       \


OBJ_FILES = $(OBJ_FILES_SUB)


INCLUDES  = \
	-I$(lib_path) -I$(CRTM_path)/include

LIBRARIES = \
	-L$(CRTM_path)/lib -lCRTM

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption) #-C -traceback -nowarn



#----------------
# Execution
#----------------
all:
	@case `uname -s` in \
		"SunOS"  ) make -f $(MAKEFILE) the_library $(SUNOS_FLAGS)  ;; \
		"AIX"    ) make -f $(MAKEFILE) the_library $(AIX_FLAGS)    ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_library $(IRIX64_FLAGS) ;; \
		"Linux"  ) make -f $(MAKEFILE) the_library $(LINUX_FLAGS)  ;; \
		*        ) echo "This system is not supported"             ;; \
	esac

the_library: lib


#----------------------------------
# Library files compilation
#----------------------------------

#----Self-dependent modules
$(lib_path)/kinds.o     : $(lib_path)/kinds.f90
	cd $(lib_path) ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) kinds.f90

$(lib_path)/Consts.o     : $(lib_path)/Consts.f90
	cd $(lib_path) ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) Consts.f90

$(lib_path)/misc.o          : $(lib_path)/misc.f90 $(lib_path)/Consts.o
	cd $(lib_path) ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) misc.f90
#----Modules dependent on others
$(lib_path)/atms_spatial_average_mod.o     : $(lib_path)/atms_spatial_average_mod.f90 \
	$(lib_path)/misc.o $(lib_path)/kinds.o
	cd $(lib_path) ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) atms_spatial_average_mod.f90

$(lib_path)/IO_DEP.o        : $(lib_path)/IO_DEP.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o $(lib_path)/utils.o $(lib_path)/ErrorHandling.o
	cd $(lib_path)/ ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_DEP.f90

$(lib_path)/ErrorHandling.o       : $(lib_path)/ErrorHandling.f90 $(lib_path)/misc.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) ErrorHandling.f90

$(lib_path)/mathFcts.o      : $(lib_path)/mathFcts.f90 $(lib_path)/ErrorHandling.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) mathFcts.f90

$(lib_path)/IO_Noise.o      : $(lib_path)/IO_Noise.f90 $(lib_path)/misc.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Noise.f90

$(lib_path)/Noise.o      : $(lib_path)/Noise.f90 $(lib_path)/misc.o \
	$(lib_path)/IO_Noise.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) Noise.f90

$(lib_path)/QCchecking.o          : $(lib_path)/QCchecking.f90 $(lib_path)/misc.o \
	$(lib_path)/CntrlParams.o $(lib_path)/TuningParams.o $(lib_path)/SeFeErrCov.o \
	$(lib_path)/GeophCovBkg.o $(lib_path)/Noise.o $(lib_path)/IO_Scene.o  \
	$(lib_path)/utils.o $(lib_path)/ErrorHandling.o $(lib_path)/Consts.o
	cd $(lib_path)/ ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) QCchecking.f90

$(lib_path)/utils.o: $(lib_path)/utils.f90 $(lib_path)/Consts.o \
	$(lib_path)/ErrorHandling.o $(lib_path)/mathFcts.o \
	$(lib_path)/Noise.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) utils.f90 

$(lib_path)/IO_Misc.o        : $(lib_path)/IO_Misc.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o $(lib_path)/utils.o $(lib_path)/ErrorHandling.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Misc.f90

$(lib_path)/IO_Misc_LE.o      : $(lib_path)/IO_Misc_LE.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o $(lib_path)/utils.o $(lib_path)/ErrorHandling.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Misc_LE.f90

$(lib_path)/IO_Scene.o        : $(lib_path)/IO_Scene.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o $(lib_path)/utils.o $(lib_path)/ErrorHandling.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Scene.f90

$(lib_path)/IO_SceneAMV.o        : $(lib_path)/IO_SceneAMV.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o $(lib_path)/utils.o $(lib_path)/ErrorHandling.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_SceneAMV.f90

$(lib_path)/IO_InstrConfig.o    : $(lib_path)/IO_InstrConfig.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_InstrConfig.f90

$(lib_path)/IO_MeasurData.o       : $(lib_path)/IO_MeasurData.f90 $(lib_path)/misc.o \
	$(lib_path)/Consts.o $(lib_path)/ErrorHandling.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS)  $(FC_FLAGS) IO_MeasurData.f90

$(lib_path)/PreClassif.o  : $(lib_path)/PreClassif.f90 \
	$(lib_path)/misc.o $(lib_path)/Consts.o
	cd $(lib_path) ; $(FC)  $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) PreClassif.f90

$(lib_path)/FwdOperator.o     : $(lib_path)/FwdOperator.f90 \
	$(lib_path)/IO_Scene.o $(lib_path)/Consts.o \
	$(lib_path)/utils.o $(lib_path)/ErrorHandling.o \
	$(lib_path)/TraceGas.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) FwdOperator.f90

$(lib_path)/TraceGas.o     : $(lib_path)/TraceGas.f90 \
	$(lib_path)/ErrorHandling.o $(lib_path)/IO_Scene.o \
	$(lib_path)/Consts.o $(lib_path)/misc.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) TraceGas.f90

$(lib_path)/CntrlParams.o     : $(lib_path)/CntrlParams.f90 \
	$(lib_path)/misc.o $(lib_path)/Consts.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) CntrlParams.f90

$(lib_path)/TuningParams.o     : $(lib_path)/TuningParams.f90 \
	$(lib_path)/misc.o $(lib_path)/Consts.o 
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) TuningParams.f90

$(lib_path)/GeophCovBkg.o     : $(lib_path)/GeophCovBkg.f90 \
	$(lib_path)/IO_Scene.o $(lib_path)/TuningParams.o \
	$(lib_path)/utils.o $(lib_path)/ErrorHandling.o \
	$(lib_path)/misc.o  $(lib_path)/CntrlParams.o
	cd $(lib_path) ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) GeophCovBkg.f90

$(lib_path)/InversProcess/SeFeErrCov.o : $(lib_path)/SeFeErrCov.f90 \
	$(lib_path)/misc.o $(lib_path)/CntrlParams.o $(lib_path)/ErrorHandling.o
	cd $(lib_path)/; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) SeFeErrCov.f90


#---Dependence 
lib : $(OBJ_FILES_SUB)



# --------
# Clean up
# --------
clean:
	$(REMOVE)  \
	$(OBJ_FILES) *.mod *.MOD *.stb *~   $(lib_path)/*.mod  $(lib_path)/*~ 




