#!/bin/ksh
set -x
mac=$(hostname | cut -c1-2)
if [ $mac = s4 ] ; then
 export machine=s4-cardinal
 export FC=ifort
 export FFLAGSM="-O3 -free -convert big_endian -traceback"
 export LIBDIR=/data/users/yzhou/osse/da_tools/nwprod_2015q1/lib
 export LIBSM="-L${LIBDIR} -lw3emc_v2.2.0_4 -lw3nco_4 -lbacio_4"
fi
make -f Makefile

