FFLAGS	= $(FFLAGSM)
LIBS	= $(LIBSM)
gribextrm :rdgrib.o  read_grib.o  grib_dist.o gribdifs.o 
	$(FC) $(FFLAGS) $(LDFLAGSM) rdgrib.o  read_grib.o  grib_dist.o gribdifs.o $(LIBS) -o gribextrm
rdgrib.o:rdgrib.f90
	$(FC) $(FFLAGS) -c rdgrib.f90
read_grib.o:read_grib.f90
	$(FC) $(FFLAGS) -c read_grib.f90
grib_dist.o:grib_dist.f90
	$(FC) -I$(INCS) $(FFLAGS) -c grib_dist.f90
gribdifs.o:gribdifs.f90
	$(FC) -I$(INCS) $(FFLAGS) -c gribdifs.f90
