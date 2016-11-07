#!/bin/sh
set -x

##-------------------------------------------------------------------
## V. Krishna Kumar/NCO/PMB April 2011 - This is a truncated version of EMC's 
## Verification System Data Base (VSDB) modified for the purpose
## of computing Forecast (GFS) - Forecast (ECMWF) correlation based
## on the principle of computing the traditional Anomaly Correlation (AC)
## Scores between different models, for example GFS and ECMWF or GFS and
## UKMET etc. For the F-F correlation, we use ECMWF forecasts as verifying
## analyses for the GFS using the AC method. Please read the original comments 
## below from Fanglin Yang/EMC/Global Modeling Branch
## Fanglin Yang,  June 2008
## E-mail: fanglin.yang@noaa.gov, Tel: 301-6833722
## Global Weather and Climate Modeling Branch, EMC/NCEP/NOAA/
## 1. This package generates forecast perfomance stats in VSDB format 
##    and makes a variety of graphics to compare anomaly correlation 
##    and RMSE among different experiments. It also makes graphics of
##    CONUS precip skill scores and fits to rawindsonde observations.
##    The different components can be turned on or off as desired. 
##    Graphics are sent to a web server for display (for example:  
##    http://www.emc.ncep.noaa.gov/gmb/wx24fy/vsdb_glopara/pru8/)
## 2. The script can handle experiments executed on different machines.
##    Users only need to specify the name, directory and the computer 
##    name from which an experiment is executed.  
## 3. Credit and Acknowledgment:
##    Binbin Zhou provided the source script for creating VSDB database.
##    Suranjana Saha provided the source script for making fit-to-obs 
##    maps. Yujian Zhu provided the source script for making precip 
##    skill score maps. All source codes and scripts have been extensively
##    modified to be included in this package. Shrinivas Moorthi helped 
##    with adding the script to port forecast data from different machines.  
##    Glenn White, Steve Lord and Xu Li made suggestions for creating and 
##    improving the significance test for AC dieoff curves and RMSE growth
##    curves. Russ Treaton modified the GrADS code for making precip maps
##    and made suggestions to include consensus analysis for verification.
##    
##-------------------------------------------------------------------

MAKEVSDBDATA=YES           ;#To create VSDB date
#MAKEVSDBDATA=NO            ;#VSDB data already exists

batch=no                   ;#to run jobs in batch mode

#----------------------------------------------------------------------
export ACCOUNT=GFS-MTN                                  ;#ibm computer ACCOUNT task
export CUE2RUN=dev                                      ;#dev or devhigh or 1
export webhost=lnx321.ncep.noaa.gov                     ;#host for web display
export webhostid=sdm                                    ;#login id on webhost 

export ftpdir=/home/nco/sdm/sdm/fcstdiv/main ;#where maps are displayed on webhost            
export doftp="NO"                                        ;#whether or not to sent maps to ftpdir

export vsdbsave=/gpfs/gd2/emc/hwrf/noscrub/$LOGNAME/fcstdiv/vsdb_data        ;#place where vsdb database is saved
export vsdbhome=${vsdbhome:-/gpfs/gd2/emc/hwrf/noscrub/$LOGNAME/fcstdiv}     ;#home
export canldir=/global/shared/stat/canl                  ;#consensus analysis directory
export canldir=/global/noscrub/Fanglin.Yang/stat/canl    ;#consensus analysis directory

### --------------------------------------------------------------
###   make vsdb database
      if [ $MAKEVSDBDATA = YES ] ; then
### --------------------------------------------------------------
export cyclist="$cyc"                             ;#cycle to verify
export expnlist="pra"                             ;#experiment names 
export expdlist="/nco/$grp/f-f-divergence/grib_files"         ;#exp directories
export complist="cirrus"                          ;#computers where experiments are run
###export complist="stratus"                        ;#computers where experiments are run

export anl_type=gfs                               ;#analysis type for verification: gfs, gdas or canl
if [ "$cyclist" -eq '00' ]; then
   export spdy=`date +%Y%m%d`
elif [ "$cyclist" -eq '12' ]; then
   export spdycyc=`/nwprod/util/exec/ndate -24 $(date +%Y%m%d)$cyclist`
   export spdy=`echo $spdycyc | cut -c1-8`
else
   echo "ECWMF GRIB files for cycle t${cyclist}z not available for F-F correlation calculations"
   exit
fi

### --------------------------------------------------------------
###   make vsdb database
      if [ $MAKEVSDBDATA = YES ] ; then
### --------------------------------------------------------------
export fcyclist="$cyc"                        ;#forecast cycles to be verified
export expnlist="gfs"                   ;#experiment names
export expdlist="/gpfs/gd2/emc/hwrf/noscrub/$LOGNAME/fcstdiv/grib_files"              ;#exp directories, can be different
export complist="$chost"              ;#computer names, can be different if passwordless ftp works
export dumplist=".gfs."                  ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}

export anl_type=gfs
if [ "$cyclist" -eq '00' ]; then
   export spdy=`date +%Y%m%d`
elif [ "$cyclist" -eq '12' ]; then
   export spdycyc=`/nwprod/util/exec/ndate -24 $(date +%Y%m%d)$cyclist`
   export spdy=`echo $spdycyc | cut -c1-8`
else
   echo "ECWMF GRIB files for cycle t${cyclist}z not available for F-F correlation calculations"
   exit

export vlength=120                        ;#forecast length in hour
export fcstdtcy=`/nwprod/util/exec/ndate +$vlength ${spdy}${cyclist}`
fcstdate=`echo $fcstdtcy | cut -c 1-8`
export DATEST=$spdy                       ;#forecast starting date
export DATEND=$fcstdate                   ;#forecast ending date
#
# For retrospective runs
#export DATEST=20101101                   ;#forecast starting date
#export DATEND=20101205                   ;#forecast ending date
#
export asub=a                             ;#string in pgb anal file after pgb, say, pgbanl, pgbhnl
export fsub=f                             ;#string in pgb fcsy file after pgb, say, pgbf06, pgbh0


export rundir=/stmpd2/$LOGNAME/f-f-divergence/vsdb_exp
#
mkdir -p $rundir
#
export listvar1=fcyclist,expnlist,expdlist,complist,dumplist,vhrlist,DATEST,DATEND,vlength,rundir,APRUN
export listvar2=machine,anl_type,iauf00,scppgb,sfcvsdb,canldir,ecmanldir,vsdbsave,vsdbhome,gd,NWPROD,batch
export listvar="$listvar1,$listvar2"

## pgb files must be saved as $expdlist/$expnlist/pgbf${fhr}${cdump}${yyyymmdd}${cyc}
if [ $batch = YES ]; then
  $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/N -r 2048/1 -t 6:00:00 \
     -j vstep1 -o $tmpdir/vstep1.out  ${vsdbhome}/scripts/verify_exp_step1.sh
else
     ${vsdbhome}/scripts/verify_exp_step1.sh 1>${tmpdir}/vstep1.out 2>&1
fi

### --------------------------------------------------------------
      fi
### --------------------------------------------------------------

export listvar=vsdbsave,vsdbhome,cyclist,expnlist,expdlist,complist,DATEST,DATEND,vlength,anl_type,rundir,canldir,asub,fsub

exit

