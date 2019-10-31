#!/bin/ksh
set -ux

##-------------------------------------------------------------------
## Fanglin Yang,  September 2010
## E-mail: fanglin.yang@noaa.gov, Tel: 301-6833722          
## Global Weather and Climate Modeling Branch, EMC/NCEP/NOAA/
##    This package generates forecast perfomance stats in VSDB format 
##    and makes a variety of graphics to compare anomaly correlation 
##    and RMSE among different experiments. It also makes graphics of
##    CONUS precip skill scores and fits to rawindsonde observations.
##    The different components can be turned on or off as desired. 
##    Graphics are sent to a web server for display (for example:  
##    http://www.emc.ncep.noaa.gov/gmb/wx24fy/vsdb/prhs11/)
##-------------------------------------------------------------------

MAKEMAPS=YES               ;#To make AC and RMS maps

#----------------------------------------------------------------------
export machine=WCOSS_C              ;#IBM(cirrus/stratus), ZEUS, GAEA, and JET etc
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
#myhome=/global/save/$LOGNAME/GFDPT/scripts/ffcorr
myhome=$GFDPTSCRIPTDIR/ffcorr
set -a;. ${myhome}/setup_envs.sh $machine 
if [ $? -ne 0 -o $rc -gt 0 ]; then exit; fi
set -ux
NWPROD=$vsdbhome/nwprod
#export ndate=${ndate:-$NWPROD/util/exec/ndate}
export webhostid=kkumar
export ftpdir=/home/people/emc/www/htdocs/GFDPT/$webhostid/fcstdiv
###

export origdate=$spdy

if [ "$cyc" -eq '00' ]; then
echo $cyc
echo $spdy
# AFEauto  export spdy=`date +%Y%m%d`
#  export tmpdir=$STMP/$LOGNAME/nwpvrfy${spdy}${cyc}               ;#temporary directory for running verification
#  export mapdir=$tmpdir/web                            ;#local directory to display plots and web templates
elif [ "$cyc" -eq '12' ]; then
#AFEauto  export spdycyc=`/nwprod/util/exec/ndate -24 $(date +%Y%m%d)$cyc`
#  export spdycyc=`/nwprod/util/exec/ndate -24 $spdy$cyc`
#  export spdycyc=`$ndate -24 $spdy$cyc`
  export spdycyc=`$NDATE -24 $spdy$cyc`
  export spdy=`echo $spdycyc | cut -c1-8`
else
  echo "ECWMF GRIB files for cycle t${fcycle}z not available for F-F correlation calculations"
  exit
fi


export tmpdir=$STMP/$LOGNAME/nwpvrfy${spdy}${cyc}               ;#temporary directory for running verification
export mapdir=$tmpdir/web                            ;#local directory to display plots and web templates
###
mkdir -p $tmpdir ||exit
if [ ! -d $mapdir ]; then
 mkdir -p $mapdir ; cd $mapdir ||exit
 tar xvf ${vsdbhome}/vsdb_exp_webpage.tar 
fi
cd $tmpdir ||exit
rm *.out

##export batch=NO

### --------------------------------------------------------------
###   make AC and RMSE maps            
      if [ $MAKEMAPS = YES ] ; then
### --------------------------------------------------------------
#
export fcycle="$cyc"                         ;#forecast cycles to be verified
export mdlist="pra"                ;#experiment names, up to 10, to compare on maps
export caplist="PRA$cyc"              ;#captions of experiments shown in plots

#if [ "$fcycle" -eq '00' ]; then
#  echo $spdy 
#elif [ "$fcycle" -eq '12' ]; then
#  export spdycyc=`/nwprod/util/exec/ndate -24 $spdy$fcycle`
#  export spdy=`echo $spdycyc | cut -c1-8`
#else
#  echo "ECWMF GRIB files for cycle t${fcycle}z not available for F-F correlation calculations"
#  exit
#fi

export ICDATE=$spdy$cyc                         ;# Jan 3,2017 00Z SH Dropout
myarch=$FFCORRDIR/$ICDATE              ;#archive directory of experiments
export vsdbsave=$FFCORRDIR/$ICDATE/vsdb_data  ;#place where vsdb database is saved
COMROT=$PTMP/$LOGNAME/                         ;#running directory of experiments
chost=$(hostname)                              ;#current computer host name
#NDATE=/nwprod/util/exec/ndate
#
export expdlist="$myarch"              ;#exp directories, can be different
export vlength=240                        ;#forecast length in hour
#export fcstdtcy=`/nwprod/util/exec/ndate +$vlength ${spdy}${fcycle}`
#export fcstdtcy=`$ndate +$vlength ${spdy}${fcycle}`
export fcstdtcy=`$NDATE +$vlength ${spdy}${fcycle}`
fcstdate=`echo $fcstdtcy | cut -c 1-8`
export vsdblist="$vsdbsave"       ;#vsdb stats directories 
export vhrlist="$cyc"                        ;#verification hours for each day to show on map
export DATEST=$spdy                     ;#verification starting date to show on map
export DATEND=$fcstdate                     ;#verification ending date to show on map
export maptop=10                           ;#can be set to 10, 50 or 100 hPa for cross-section maps
export maskmiss=1                          ;#remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
export rundir=$tmpdir/acrms$$
export scoredir=$rundir/score
export scorecard=NO
export waitscore=0300

  ${vsdbhome}/verify_exp_step2.sh  1>${tmpdir}/vstep2.out 2>&1 
  pwd
  grep  -Po 'Job <\K[^>]*' ${tmpdir}/vstep2.out > jobids
  cat jobids >> $GFDPTTMPDIR/jobids.$spdy$cyc
  cat jobids >> $GFDPTTMPDIR/jobids.$origdate

iter=10
while [ $iter -gt 0 ]; do

echo $iter

((iter--))
done

### --------------------------------------------------------------
      fi
### --------------------------------------------------------------
exit
