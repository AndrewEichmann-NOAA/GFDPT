#!/bin/ksh
set -ux

## set up common directories, utilities and environment variables
## for different platforms, and assign user specific parameters.

machine=${1:-WCOSS_C}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
export rc=0

#==================================
## machine-independent parameters
#==================================
export anl_type=gfs            ;#analysis type: gfs, gdas, ecmwf, manl, canl, or fcst00, fcst120 etc
#export anl_type=ecmwf            ;#analysis type: gfs, gdas, ecmwf, manl, canl, or fcst00, fcst120 etc
                                ##gfs/gdas--own anl of each exps, manl--mean in expnlist; canl--mean of GFS,EC and UK.
export iauf00="NO"             ;#for forecasts using IAU method, force pgbf00=pgbanl
export sfcvsdb="YES"           ;#include the group of surface variables       
export gd=G2                   ;#grid resoultion on which vsdb stats are computed, G2->2.5deg, G3->1deg, G4->0.5deg
#export doftp="YES"             ;#whether or not to send maps to web server
export doftp="NO"             ;#whether or not to send maps to web server
export scppgb="NO"             ;#copy files between machine? need passwordless ssh
#export batch="YES"             ;#run jobs at batch nodes                              
export batch="NO"             ;#run jobs at batch nodes                              
export scorecard="NO"          ;#create scorecard text files and web display plate                          
if [ $machine != WCOSS_C -a $machine != WCOSS -a $machine != WCOSS_D ]; then 
 export doftp="NO"
fi

#==================================
## user-specific parameters
#==================================
# (other machine options cut out
#----------------------------
if [ $machine = WCOSS_C ]; then
#elif [ $machine = WCOSS_C ]; then
 chost=`echo $(hostname) |cut -c 1-1`
 export vsdbsave=/gpfs/hps3/emc/global/noscrub/$LOGNAME/archive/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=GFS-T2O                                ;#ibm computer ACCOUNT task
 export CUE2RUN=dev                                    ;#dev or dev_shared         
 export CUE2FTP=dev_transfer                           ;#queue for data transfer
 export GROUP=g01                                      ;#group of account, g01 etc
 export nproc=24                                       ;#number of PEs per node   
 export cputime=10:00:00                               ;#CPU time hh:mm:ss to run each batch job
 export MPMD=YES
#----------------------------
#----------------------------
else
 echo "machine $machine is not supportted by NCEP/ECM"
 echo "Please first install the verification package. exit" 
 export rc=1
 exit
fi


if [ $doftp = YES ]; then
  export webhost=emcrzdm.ncep.noaa.gov     ;#host for web display
  export webhostid=$LOGNAME                ;#login id on webhost 
  export ftpdir=/home/people/emc/www/htdocs/gmb/$webhostid/vsdb   ;#where maps are displayed on webhost            
fi 

#=====================================
## common machine-dependent parameters
#=====================================
#----------------------------
if [ $machine = WCOSS_C ]; then
 chost=`echo $(hostname) |cut -c 1-1`
 export vsdbhome=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/VRFY/vsdb     ;#script home, do not change
 export obdata=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/obdata          ;#observation data for making 2dmaps
 export gstat=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat             ;#global stats directory              
 export gfsvsdb=$gstat/vsdb_data                            ;#operational gfs vsdb database
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                               ;#observed precip for verification
 export gfswgnedir=$gstat/wgne                              ;#operational gfs precip QPF scores
 export gfsfitdir=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat/fit2obs     ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_wcoss_c                    ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                             ;#common utilities and libs included in /nwprod
 export GNOSCRUB=/gpfs/hps3/emc/global/noscrub               ;#archive directory                          
 export STMP=/gpfs/hps3/stmp                                 ;#temporary directory                          
 export PTMP=/gpfs/hps3/ptmp                                 ;#temporary directory                          
 export GRADSBIN=/usrx/local/dev/GrADS/2.0.2/bin            ;#GrADS executables       
 export IMGCONVERT=/usr/bin/convert                         ;#image magic converter
 export FC=/opt/intel/composer_xe_2015.3.187/bin/intel64/ifort    ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"                 ;#intel compiler options
 export APRUN="aprun -n 1 -N 1 -j 1 -d 1"                   ;#affix to run batch jobs   

fi

