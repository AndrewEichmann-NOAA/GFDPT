#!/bin/sh
set -x

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#     NCEP EMC GLOBAL MODEL VERIFICATION SYSTEM
#            Fanglin Yang, Dec 2006
#
#  1. This script verifies and compares forecasts from different cycles 
#     of one model and/or from different models for the same cycle. 
#  2. Statistics are extracted/derived from a VSDB data base. 
#  3. For verification vtype=anom, anomalies are derived for forecasts 
#     from all models/cycles using the same NCEP CDAS 30-year 
#     climatology (/nwprod/fix/cmean_1d.x) before computing scores.
#----------------------------------------------------------------------
#----------------------------------------------------------------------
echo `date`
export vsdb_data=/nco/$grp/f-f-divergence/vsdb_data
export vsdb_scripts=/nco/$grp/f-f-divergence/scripts
export rundir=/stmp/$LOGNAME/f-f-divergence/vsdb_stats
mkdir -p $rundir
export archmon=NO                                        ;# archive monthly means 
export webhost="lnx321.ncep.noaa.gov"
export webhostid=sdm
export account=GFS-MTN
export cue2run=${cue2run:-dev}
#export batch=${batch:-yes}
export batch=${batch:-no}
export tsleep=${tsleep:-10}
if [ $batch = no ]; then sleep $tsleep ; fi                 ;# wait for VSDB database to be created


## -- verification dates
CDATE=${1:-$(date +%Y%m%d)}
spdy=$CDATE
vlength=120
dcyc=00
export fcstdtcy=`/nwprod/util/exec/ndate +$vlength ${spdy}${dcyc}`
fcstdate=`echo $fcstdtcy | cut -c 1-8`
edate=$fcstdate      ;#end of verification date
ndays=36          ;#number of days back for verification
fdays=5          ;#forecast length in days to be verified 

export listvar1=edate,ndays,fdays,vsdb_data,sorcdir,rundir,archmon,webhost,webhostid
#---------------------------------------
#---------------------------------------
#---------------------------------------
#2.  Anomaly, one model, multiple cycles
#---------------------------------------
#---------------------------------------
export ftpdir=/home/nco/sdm/sdm/fcstdiv/main ;#where maps are displayed on webhost
export doftp="YES"
export mapdir=$rundir/maps
mkdir -p $mapdir
export copymap="NO"
#export cyclist="00 06 12 18"
export cyclist="00 12"
export mdlist="pra"
export listvar2=$listvar1,ftpdir,doftp,mapdir,copymap,cyclist,mdlist 

# ----------------------
# anomaly verification 
    export vtype=anom 
    export listvar3=$listvar2,vtype
# ----------------------
    export vnamlist="HGT"
    export reglist="G2 G2/TRO G2/NHX G2/SHX"
    export levlist="P1000 P700 P500 P250"
    export listvar=$listvar3,vnamlist,reglist,levlist 
    if [ $batch = yes ]; then
     sub -e $listvar -a $account  -q $cue2run -p 1/1 -t 3:00:00 -j HGT_gfs -o $rundir/HGT_gfs.out \
           ${vsdb_scripts}/onecenter_ncyc.sh $edate $ndays $fdays
    else
           ${vsdb_scripts}/onecenter_ncyc.sh $edate $ndays $fdays  > $rundir/HGT_gfs.out &
    fi

    export vnamlist="WIND U V"
    export reglist="G2 G2/TRO G2/NHX G2/SHX"
    export levlist="P850 P500 P250"
    export listvar=$listvar3,vnamlist,reglist,levlist 
    if [ $batch = yes ]; then
    sub -e $listvar -a $account  -q $cue2run -p 1/1 -t 3:00:00 -j WIND_gfs -o $rundir/WIND_gfs.out \
           ${vsdb_scripts}/onecenter_ncyc.sh $edate $ndays $fdays
    else
           ${vsdb_scripts}/onecenter_ncyc.sh $edate $ndays $fdays   > $rundir/WIND_gfs.out &
    fi

    export vnamlist="T"
    export reglist="G2 G2/TRO G2/NHX G2/SHX"
    export levlist="P850 P500 P250"
    export listvar=$listvar3,vnamlist,reglist,levlist 
    if [ $batch = yes ]; then
     sub -e $listvar -a $account  -q $cue2run -p 1/1 -t 3:00:00 -j T_gfs -o $rundir/T_gfs.out \
           ${vsdb_scripts}/onecenter_ncyc.sh $edate $ndays $fdays
    else
           ${vsdb_scripts}/onecenter_ncyc.sh $edate $ndays $fdays   > $rundir/T_gfs.out &
    fi


#----------------------------------------------------------
exit
