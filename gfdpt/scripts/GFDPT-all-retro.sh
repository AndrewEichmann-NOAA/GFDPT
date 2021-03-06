#!/bin/sh
set -x

. /etc/bash.bashrc

source ~/.bashrc # this needs to be stripped down to essentials

#module load ncep

export PRODDEV=dev


export basespdy=`date +%Y%m%d`
#export basespdy=


export GFDPTDIR=/gpfs/hps3/emc/global/noscrub/Andrew.Eichmann/GFDPT/code/$PRODDEV/EMC_gfdpt/gfdpt/
export GFDPTSCRIPTDIR=$GFDPTDIR/scripts
export TEMPLATESDIR=$GFDPTDIR/templates

#export GFDPTDATADIR=/global/noscrub/Andrew.Eichmann/GFDPT/$PRODDEV
export GFDPTDATADIR=/gpfs/hps3/emc/global/noscrub/Andrew.Eichmann/GFDPT/data/$PRODDEV/
export FFCORRDIR=$GFDPTDATADIR/ffcorr
export GFDPTTMPDIR=$GFDPTDATADIR/tmp
export WEBSTAGEDIR=$GFDPTDATADIR/website
export RADSCRIPTDIR=$GFDPTSCRIPTDIR/radiance
export RADDATADIR=$GFDPTDATADIR/radiance
#export TARFILEDIR=/gpfs/hps/nco/ops/com/gfs/prod/
#export TARFILEDIR=/global/noscrub/Andrew.Eichmann/tmp/
export TARFILEDIR=/gpfs/dell1/nco/ops/com/gfs/prod/

export REMOTEWEBDIR=/home/people/emc/aeichmann/GFDPT/$PRODDEV
export REMOTEWEBUSER=aeichmann
export REMOTEWEBSERVER=vm-lnx-emcrzdm01.ncep.noaa.gov

export NDATE=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate
export COPYGB=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/copygb
export LS=/bin/ls

export DATASOURCES="amsua_n15 amsua_n18 amsua_n19 amsua_metop-a amsua_metop-b atms_npp"
#export DATASOURCES="amsua_n15 "

source $GFDPTSCRIPTDIR/setenv.sh

export spdy=$basespdy
export spdy00z=${basespdy}00
export spdy12z=`$NDATE -12 $spdy00z`

#module load lsf # for bjobs


makeup=$FFCORRDIR/missingdates
rm ${makeup}
#
#python getter.py
#
#while IFS= read -r date
#do
#   export spdy=`echo $date | cut -b 1-8`
#   echo $date
#   echo $spdy
#   sh $GFDPTSCRIPTDIR/GFDPT-ffcorr.sh >& $GFDPTTMPDIR/${spdy}.out &
##   sh $GFDPTSCRIPTDIR/GFDPT-ffcorr.sh 
#done < "$makeup"
#
#sleep 1m
#
#
spdy=$basespdy
#sh $GFDPTSCRIPTDIR/GFDPT-ffcorr.sh 

#exit


#sh $GFDPTSCRIPTDIR/GFDPT-ffplot.sh
#exit
#sh $GFDPTSCRIPTDIR/GFDPT-ffcorr-makeweb.sh
#exit

#sh $GFDPTSCRIPTDIR/GFDPT-gribext.sh
#exit


#sh $GFDPTSCRIPTDIR/GFDPT-run-plotrad.sh

#exit
sh $GFDPTSCRIPTDIR/GFDPT-errorevo.sh

