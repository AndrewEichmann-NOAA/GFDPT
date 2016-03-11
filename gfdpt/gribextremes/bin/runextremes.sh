#!/bin/sh

#export execfile=/data/users/kkumar/GFDPT-Project/fcstdiv/sorc/gribextremes.gfs.ecmwf.fd/gribextrm
export execfile=../src/gribextrm
export input_ges=pgbf06.gfs.2013070118
export input_gfs=pgbanl.gfs.2013070200
export input_ecmwf=pgbanl.ecm.2013070200

$execfile  > rdgrib.gfs.ec.ges 2> rdgrib_outout
