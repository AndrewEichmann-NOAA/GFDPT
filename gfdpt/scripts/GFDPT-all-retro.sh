#!/bin/sh
set -x

#source ~/.bashrc # this needs to be stripped down to essentials


export PRODDEV=dev


export spdy=`date +%Y%m%d`
#export spdy=20190429


#export GFDPTDIR=/global/save/Andrew.Eichmann/GFDPT/$PRODDEV
export GFDPTDIR=/gpfs/hps3/emc/global/noscrub/Andrew.Eichmann/GFDPT/code/$PRODDEV
export GFDPTSCRIPTDIR=$GFDPTDIR/scripts
export TEMPLATESDIR=$GFDPTDIR/templates

#export GFDPTDATADIR=/global/noscrub/Andrew.Eichmann/GFDPT/$PRODDEV
export GFDPTDATADIR=/gpfs/hps3/emc/global/noscrub/Andrew.Eichmann/GFDPT/data/$PRODDEV
export FFCORRDIR=$GFDPTDATADIR/ffcorr
export GFDPTTMPDIR=$GFDPTDATADIR/tmp
export WEBSTAGEDIR=$GFDPTDATADIR/website
export RADSCRIPTDIR=$GFDPTSCRIPTDIR/radiance
export RADDATADIR=$GFDPTDATADIR/radiance
export TARFILEDIR=/gpfs/hps/nco/ops/com/gfs/prod/
#export TARFILEDIR=/global/noscrub/Andrew.Eichmann/tmp/

export REMOTEWEBDIR=/home/people/emc/aeichmann/GFDPT/$PRODDEV
export REMOTEWEBUSER=aeichmann
export REMOTEWEBSERVER=vm-lnx-emcrzdm01.ncep.noaa.gov

export DATASOURCES="amsua_n15 amsua_n18 amsua_n19 amsua_metop-a amsua_metop-b atms_npp"

source $GFDPTSCRIPTDIR/setenv.sh

#module load lsf # for bjobs

sh $GFDPTSCRIPTDIR/GFDPT-ffcorr.sh
exit
sh $GFDPTSCRIPTDIR/GFDPT-ffplot.sh
#exit
sh $GFDPTSCRIPTDIR/GFDPT-ffcorr-makeweb.sh
exit

#sh $GFDPTSCRIPTDIR/GFDPT-gribext.sh
#exit
sh $GFDPTSCRIPTDIR/GFDPT-gribext-makeweb.sh
#exit


sh $GFDPTSCRIPTDIR/GFDPT-run-plotrad.sh
#exit

####sh $GFDPTSCRIPTDIR/GFDPT-clim.sh

#exit
sh $GFDPTSCRIPTDIR/GFDPT-errorevo.sh

