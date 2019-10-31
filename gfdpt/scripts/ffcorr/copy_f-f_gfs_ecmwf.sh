#!/bin/sh
set -x

##-------------------------------------------------------------------------
## V. Krishna Kumar, March 20, 2017 
## E-mail: Krishna.Kumar@noaa.gov, Tel: 301-683-3663
## JCSDA/NESDIS
##
## 1. This script copies all the required pgb files of GFS and ECM 
##    from EMC's verification data base /global/noscrub/emc.glopara/global/gfs (GFS)
##    /global/noscrub/emc.glopara/global/ecm (ECMWF) to the running directory for computing
##    the forecast-forecast correlation statistics between GFS vs ECMWF for
##    a given $PDY, $cyc and $vlength (forecast length)
##-------------------------------------------------------------------------

# XXX means the env vars should be rationalized

export vsdbhome=$FFCORRDIR    ;# home
#export exechome=${execdir:-/global/save/$LOGNAME/GFDPT}    ;# home
export exechome=$GFDPTDIR
export exp1=pra
export exp2=ecm
export expg=gfs
#export pgbgfsdir=/global/noscrub/emc.glopara/global/${expg}
#export pgbecmdir=/global/noscrub/emc.glopara/global/${exp2}
export pgbgfsdir=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat/${expg} # XXX
export pgbecmdir=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat/${exp2}  # XXX
export execdir=$exechome/exec
export cycle=t${cyc}z
#export NDATE=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate # XXX

if [ "$cyc" -eq '00' ]; then 
   export fpdy=$spdy
elif [ "$cyc" -eq '12' ]; then
   export spdycyc=`$NDATE -24 $spdy$cyc`
   export spdy=`echo $spdycyc | cut -c1-8`
   export fpdy=$spdy
else
   echo "ECWMF GRIB files for cycle t${cyc}z not available for F-F correlation calculations"
   exit
fi
 
export vlength=240
export fcst_int=24
export asub=a
export fsub=f

## pgb files must be in the form of pgb${fsub}${fhr}.yyyymmdd${cyc}

sday=0
#nday=`/nwprod/util/ush/finddate.sh $spdy s-1`
nday=`/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.31/ush/finddate.sh $spdy s-1` # XXX

while [ "$nday" -lt "$fpdy" ]
do
  sday=`expr $sday + 1`
#  nday=`/nwprod/util/ush/finddate.sh $nday d+1` 
  nday=`/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.31/ush/finddate.sh $nday d+1` # XXX
  PDY=$nday
##

  export DATA1=$vsdbhome/${PDY}${cyc}/${exp1}
  export DATA2=$vsdbhome/${PDY}${cyc}/${exp2}
  ls -d $DATA1 $DATA2
  err0=$?
  if test $err0 -eq 0
  then
   rm -rf $DATA1 $DATA2
  ls
  else
  echo "$DATA1 $DATA2 directories do not exist"
  echo "Safe to create these"
  fi
  mkdir -p $DATA1 $DATA2
##
iday=0
fday=`expr $vlength / $fcst_int`
fhr=00

while [ "$iday" -lt "$fday" ]
do
  iday=`expr $iday + 1`
  fhr=`expr $fcst_int \* $iday`
  echo "iday = " $iday  "fhr = " $fhr
  ln -s $pgbgfsdir/pgb${fsub}${fhr}.${expg}.${PDY}${cyc} $DATA1/pgb${fsub}${fhr}.${exp1}.${PDY}${cyc}
  ln -s $pgbecmdir/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc} $DATA2/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc}.o
#
# Convert the ECMWF grib headers from forecast dates to the verification dates
#       
  vdate=`wgrib -verf -4yr -d 1 $DATA2/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc}.o  | grep -i 'd=' | cut -c7-16`
  fhour=00
  p_id=81
  cat <<EOH > cardec
  ${vdate} ${p_id} ${fhour}
EOH

export FORT11="$pgbecmdir/pgb${fsub}${fhr}.${exp2}.${PDY}${cyc}"
export FORT51="$DATA1/pgb${fsub}${fhour}.${exp1}.${vdate}"
$execdir/overdate.grib.gfs.ecmwf 1 > overdate.grib.gfs.ecmwf.out < cardec 2>overdate.grib.gfs.ecmwf.err

done

done
