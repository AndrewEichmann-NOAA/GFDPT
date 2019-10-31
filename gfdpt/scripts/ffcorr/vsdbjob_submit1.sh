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

MAKEVSDBDATA=YES           ;#To create VSDB date

#----------------------------------------------------------------------

#export machine=WCOSS              ;#IBM(cirrus/stratus), ZEUS, GAEA, and JET etc
export machine=WCOSS_C              ;#IBM(cirrus/stratus), ZEUS, GAEA, and JET etc
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
vsdbhome=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/VRFY/vsdb
myhome=$GFDPTSCRIPTDIR/ffcorr
NWPROD=$vsdbhome/nwprod
#export ndate=${ndate:-$NWPROD/util/exec/ndate}

set -a;. ${myhome}/setup_envs.sh $machine 
if [ $? -ne 0 -o $rc -gt 0 ]; then exit; fi
set -ux
export webhostid=kkumar
export ftpdir=/home/people/emc/www/htdocs/GFDPT/$webhostid/main

#export tmpdir=$STMP/$LOGNAME/nwpvrfy$$               ;#temporary directory for running verification
### --------------------------------------------------------------
###   make vsdb database
      if [ $MAKEVSDBDATA = YES ] ; then
### --------------------------------------------------------------

export fcyclist="$cyc"                           ;#forecast cycles to be verified
export expnlist="pra"                      ;#experiment names
export complist="$chost"              ;#computer names, can be different if passwordless ftp works 
export dumplist=".pra."                  ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
export vhrlist="$cyc"                        ;#verification hours for each day             

if [ "$fcyclist" -eq '00' ]; then
# AFE auto   export spdy=`date +%Y%m%d`
   echo $spdy
echo $spdy
elif [ "$fcyclist" -eq '12' ]; then
#  export spdycyc=`$ndate -24 $spdy$fcyclist`
  export spdycyc=`$NDATE -24 $spdy$fcyclist`
  export spdy=`echo $spdycyc | cut -c1-8`
else
  echo "ECWMF GRIB files for cycle t${fcyclist}z not available for F-F correlation calculations"
  exit
fi
#

export tmpdir=$STMP/$LOGNAME/nwpvrfy${spdy}${cyc}               ;#temporary directory for running verification
export mapdir=$tmpdir/web                            ;#local directory to display plots and web templates
mkdir -p $tmpdir ||exit
if [ ! -d $mapdir ]; then
 mkdir -p $mapdir ; cd $mapdir ||exit
 tar xvf ${vsdbhome}/vsdb_exp_webpage.tar 
fi
cd $tmpdir ||exit
rm *.out


export ICDATE=$spdy$cyc                        ;# Jan 3,2017 00Z SH Dropout
myarch=$FFCORRDIR/$ICDATE              ;#archive directory of experiments
export vsdbsave=$FFCORRDIR/$ICDATE/vsdb_data  ;#place where vsdb database is saved
COMROT=$PTMP/$LOGNAME/                         ;#running directory of experiments
chost=$(hostname)                              ;#current computer host name
#NDATE=$NWPROD/util/exec/ndate
#
export expdlist="$myarch"              ;#exp directories, can be different
export vlength=240                        ;#forecast length in hour
#export fcstdtcy=`$ndate +$vlength ${spdy}${fcyclist}`
export fcstdtcy=`$NDATE +$vlength ${spdy}${fcyclist}`
fcstdate=`echo $fcstdtcy | cut -c 1-8`

export DATEST=$spdy                       ;#forecast starting date
export DATEND=$fcstdate                   ;#forecast ending date
#

export rundir=$tmpdir/stats
export listvar1=fcyclist,expnlist,expdlist,complist,dumplist,vhrlist,DATEST,DATEND,vlength,rundir
export listvar2=machine,anl_type,scppgb,sfcvsdb,canldir,ecmanldir,vsdbsave,vsdbhome,gd,NWPROD
export listvar="$listvar1,$listvar2"

## pgb files must be saved as $expdlist/$expnlist/pgbf${fhr}${cdump}${yyyymmdd}${cyc}
if [ $batch = YES ]; then
  $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/N -r 2048/1 -t 6:00:00 \
     -j vstep1 -o $tmpdir/vstep1.out  ${vsdbhome}/verify_exp_step1.sh
else
     ${vsdbhome}/verify_exp_step1.sh 1>${tmpdir}/vstep1.out 2>&1
fi

### --------------------------------------------------------------
      fi                                       
### --------------------------------------------------------------
exit

