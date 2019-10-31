SHELL	=/bin/sh
#
SRCS	=rdgrib.f90 read_grib.f90 grib_dist.f90 gribdifs.f90
#
OBJS	=rdgrib.o  read_grib.o  grib_dist.o gribdifs.o
#
FC	=ifort
#
LIBS	=-L/nwprod/lib -lw3emc_8 -lw3nco_8 -lsp_8 -lbacio_8
#
CMD	=gribextremes.gfs.ecmwf.x

#
# MAKEFILE OPTIONS: KRISHNA KUMAR
#
DEBUG	=-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector
#FFLAGS	= -O2 -free -convert big_endian -list -assume noold_ldout_format -fixed -g -traceback $(DEBUG)
FFLAGS	= -O2 -free -convert big_endian -list -g -traceback $(DEBUG)

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all: 		$(CMD)

$(CMD): 	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS) 

gribdifs.o:	gribdifs.f90
	$(FC) $(FFLAGS) -c gribdifs.f90

grib_dist.o:	grib_dist.f90
	$(FC) $(FFLAGS) -c grib_dist.f90

read_grib.o:	read_grib.f90
	$(FC) $(FFLAGS) -c read_grib.f90

rdgrib.o:	rdgrib.f90
	$(FC) $(FFLAGS) -c rdgrib.f90

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec
