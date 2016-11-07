#!/bin/sh
#
#  This makefile is to compile grid-to-grid all codes
#  Author: Binbin Zhou
#  Krishna Kumar April 4, 2016 Modified for WCOSS 
#


FC=      ifort
FOPTS=  -convert big_endian -heap-arrays -i4
CMD=    grid2grid.gfs.ecmwf.x

SRCS=  grid2grid.f gtgdef.f readcntl.f setarea.f \
	setfcst.f setlevel.f setmodel.f setobtyp.f status.f \
	stclst.f stldsp.f stlstr.f stnumb.f stread.f strmbl.f \
	strxbl.f getregion.f getnumber.f getGRIBdata.f \
	read_area.f sl1l2.f vl1l2.f fho.f getfho.f get_hasdata.f \
	getHGTsfc.f getTndGRIBdata.f getMeanClimData.f sal1l2.f \
	getAnomlyLevMrk.f val1l2.f afho.f FITWAV_2D.f FITWAV_1D.f \
	esl1l2.f evl1l2.f rhnt.f rhet.f
LIBS=   /nwprod/lib/libbacio_4.a \
	/nwprod/lib/libw3emc_4.a \
	/nwprod/lib/libw3nco_4.a \
	/nwprod/lib/libbufr_4_64.a \
	/nwprod/lib/libip_4.a \
	/nwprod/lib/libsp_4.a \
	/apps/esmf/3.1.0rp5/intel/mpt/lib/libO/Linux.intel.64.mpi.default -lesmf 

$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
########################################################
