#---------------------------------------------------------------------------------
# Description:
#    Makefile to compile f90 code that converts ssmis BUFR 
#    into FMSDR format
#
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Version: May 5, 2014, DXu, Initial coding
#
#----------------------------------------------
# Compile option
FC=/usr/local/intel/composerxe-2011.2.137/bin/intel64/ifort
CFLAGS = -O3 -shared-intel -convert big_endian 

# Link option
LD=$(FC)
LDFLAGS = -O3 -shared-intel -Wl,

# BUFR library location
BUFRLIB_DIR = ../bufrlib

# Clean tool
RM = rm

LOCAL_EXE = ssmisBUFR_Converter.exe
EXE = ../../../bin/ssmisBUFR_Converter.exe
FSRC = kinds.f90 constants.f90  Consts.f90 misc.f90 ErrorHandling.f90\
       IO_MeasurData.f90 fmsdr_struct.f90 ssmisBUFR_Converter.f90
FOBJ = ${FSRC:.f90=.o}

# Clear out all suffixes 
.SUFFIXES:
# List only those we use
.SUFFIXES: .o .f90

# Define a suffix rule for .f90 -> .o
.f90.o: 
	$(FC) $(CFLAGS) -c $<

all : $(EXE) $(LOCAL_EXE)

$(EXE): $(FOBJ) $(BUFRLIB_DIR)/libbufr.a
	$(LD) $(LDFLAGS) -o $@ $(FOBJ)  -L$(BUFRLIB_DIR)  -lbufr

$(LOCAL_EXE): $(FOBJ) $(BUFRLIB_DIR)/libbufr.a
	$(LD) $(LDFLAGS) -o $@ $(FOBJ)  -L$(BUFRLIB_DIR)  -lbufr

clean:
	$(RM) -f  *.o *.mod 

cleaner:
	$(RM) -f $(EXE) $(LOCAL_EXE)  *.o *.mod
