#!/bin/sh
set -x


source ~/.bashrc
module load lsf # for bjobs
exit

export PRODDEV=dev


export spdy=`date +%Y%m%d`
#export spdy=20180325

export GFDPTDIR=/global/save/Andrew.Eichmann/GFDPT/$PRODDEV
export GFDPTSCRIPTDIR=$GFDPTDIR/scripts
export TEMPLATESDIR=$GFDPTDIR/templates

export GFDPTDATADIR=/global/noscrub/Andrew.Eichmann/GFDPT/$PRODDEV
export FFCORRDIR=$GFDPTDATADIR/ffcorr
export GFDPTTMPDIR=$GFDPTDATADIR/tmp
export WEBSTAGEDIR=$GFDPTDATADIR/website
export RADSCRIPTDIR=$GFDPTSCRIPTDIR/radiance
export RADDATADIR=$GFDPTDATADIR/radiance
export TARFILEDIR=/gpfs/hps/nco/ops/com/gfs/prod/

export REMOTEWEBDIR=/home/people/emc/aeichmann/GFDPT/$PRODDEV
export REMOTEWEBUSER=aeichmann
export REMOTEWEBSERVER=vm-lnx-emcrzdm01.ncep.noaa.gov

export DATASOURCES="amsua_n15 amsua_n18 amsua_n19 amsua_metop-a amsua_metop-b
atms_npp"

module load lsf # for bjobs

sh $GFDPTSCRIPTDIR/GFDPT-ffcorr.sh
#exit
sh $GFDPTSCRIPTDIR/GFDPT-ffplot.sh
sh $GFDPTSCRIPTDIR/GFDPT-ffcorr-makeweb.sh

sh $GFDPTSCRIPTDIR/GFDPT-gribext.sh
sh $GFDPTSCRIPTDIR/GFDPT-gribext-makeweb.sh

sh $GFDPTSCRIPTDIR/GFDPT-plotrad.sh

sh $GFDPTSCRIPTDIR/GFDPT-clim.sh

sh $GFDPTSCRIPTDIR/GFDPT-errorevo.sh

