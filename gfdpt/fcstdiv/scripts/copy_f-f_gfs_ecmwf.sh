#!/bin/sh
set -x

##-------------------------------------------------------------------------
## V. Krishna Kumar, April 4, 2016 
## E-mail: Krishna.Kumar@noaa.gov, Tel: 301-683-3663
## JCSDA/NESDIS
##
## 1. This script copies all the required pgb files of GFS and ECM 
##    from EMC's verification data base /global/noscrub/emc.glopara/global/gfs (GFS)
##    /global/noscrub/emc.glopara/global/ecm (ECMWF) to the running directory for computing
##    the forecast-forecast correlation statistics between GFS vs ECMWF for
##    a given $PDY, $cyc and $vlength (forecast length)
##-------------------------------------------------------------------------

export vsdbhome=${vsdbhome:-/ptmpd2/$LOGNAME/fcstdiv}    ;# home
export exechome=${execdir:-/sss/emc/hwrf/save/$LOGNAME/fcstdiv}    ;# home
export exp1=gfs
export exp2=ecm
export pgbgfsdir=/global/noscrub/emc.glopara/global/${exp1}
export pgbecmdir=/global/noscrub/emc.glopara/global/${exp2}
export execdir=$exechome/exec
export cyc=00
export cycle=t${cyc}z

if [ "$cyc" -eq '00' ]; then 
   export spdy=`date +%Y%m%d`
   export fpdy=$spdy
elif [ "$cyc" -eq '12' ]; then
   export spdycyc=`/nwprod/util/exec/ndate -24 $(date +%Y%m%d)$cyc`
   export spdy=`echo $spdycyc | cut -c1-8`
   export fpdy=$spdy
else
   echo "ECWMF GRIB files for cycle t${cyc}z not available for F-F correlation calculations"
   exit
fi
 
#
# Set the spdy and fpdy manually for retrospective runs 
#
export spdy=20151201
export fpdy=20151205

export vlength=120
export fcst_int=24
export asub=a
export fsub=f
export DATA1=$vsdbhome/grib_files/${exp1}
export DATA2=$vsdbhome/grib_files/${exp2}
ls -d $DATA1 $DATA2
err0=$?
if test $err0 -eq 0
then
#   rm -rf $DATA1 $DATA2
ls
else
   echo "$DATA1 $DATA2 directories do not exist"
   echo "Safe to create these" 
fi
mkdir -p $DATA1 $DATA2

## pgb files must be in the form of pgb${fsub}${fhr}.${exp1/2}.yyyymmdd${cyc}

sday=0
nday=`/nwprod/util/ush/finddate.sh $spdy s-1`

while [ "$nday" -lt "$fpdy" ]
do
  sday=`expr $sday + 1`
  nday=`/nwprod/util/ush/finddate.sh $nday d+1` 
  PDY=$nday

iday=0
fday=`expr $vlength / $fcst_int`
fhr=00
#ln -sf $pgbgfsdir/pgb${fsub}${fhr}.${exp1}.${PDY}${cyc} $DATA1/pgb${fsub}${fhr}.${exp1}.${PDY}${cyc}.o
#ln -sf $pgbecmdir/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc} $DATA1/.
#ln -sf $pgbecmdir/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc} $DATA2/.

while [ "$iday" -lt "$fday" ]
do
  iday=`expr $iday + 1`
  fhr=`expr $fcst_int \* $iday`
  echo "iday = " $iday  "fhr = " $fhr
  ln -s $pgbgfsdir/pgb${fsub}${fhr}.${exp1}.${PDY}${cyc} $DATA1/.
  ln -s $pgbecmdir/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc} $DATA2/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc}.o
#
# Convert the ECMWF grib headers from forecast dates to the verification dates
#       
  vdate=`wgrib -verf -4yr -d 1 $DATA2/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc}.o | grep -i 'd=' | cut -c7-16`
#  fhour=`echo $vdate | cut -c9-10`
  fhour=00
#  p_id=`wgrib -PDS10 -d 1 $DATA1/pgb${fsub}${fhr}.${exp1}.${PDY}${cyc} | awk ' { print $12 } '`
  p_id=81
  cat <<EOH > cardec
  ${vdate} ${p_id} ${fhour}
EOH

#Input file is the original ecmwf forecast file 
export FORT11="$pgbecmdir/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc}" 
export FORT51="$DATA1/pgb${fsub}${fhour}.${exp1}.${vdate}"
$execdir/overdate.grib.gfs.ecmwf 1 > overdate.grib.gfs.ecmwf.out < cardec 2>overdate.grib.gfs.ecmwf.err

done
done
