#!/bin/sh
set -x
#####################################################################
echo "------------------------------------------------"
echo "job_gfs_ecmwf_gribextremes.sh"
echo "------------------------------------------------"
echo "V. Krishna Kumar Systems Integration Branch"
echo "History: APRIL 2017 - Implementation of this script after modifications."
#
# Script to compute GRIBEXTREMES between GFS & ECMWF GRIB files over the globe 
#
#####################################################################
# obtain unique process id (pid) and make temp directory
#####################################################################
export pid=$$
#####################################################################
# Determine Job Output Name on System
#####################################################################
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"
#
#export gribout=/stmpp2/$LOGNAME/fcstdiv/gribextremes_${NET}_${RUNM}_${pid}_${cyc}
#export gribout=/gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gribextremes_${NET}_${RUNM}_${pid}_${cyc}
export gribout=/gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gribextremes_${NET}_${RUNM}_${spdy}${cyc}
export output=$gribout/$pgmout
export error=$gribout/${jobid}_errors

export cycle=t${cyc}z
#
ls -d $gribout
err0=$?
if test $err0 -eq 0
then
#   rm -rf $gribout
  ls
else
  echo "$gribout directory does not exist"
  echo "Safe to create this"
fi
  mkdir -p $gribout
  cd $gribout
#
#####################################################################
# SENDSMS  - Flag Events on SMS
# SENDDBN  - Issue DBNet Client Calls
# SENDCOM  - Copy files to /com directory
# SENDSDM  - Send files to SDM printer (used by child script bufr_datacount.sh)
#####################################################################
export SENDSMS=NO
export SENDDBN=NO
export SENDCOM=NO
export SENDSDM=NO

####################################
# Specify Execution Areas
####################################
#export HOMEPREP=/global/$grp/$LOGNAME/r2o/fcstdiv
#export HOMESCRIPTS=$HOMEPREP/scripts
export HOMESCRIPTS=$GFDPTSCRIPTDIR/gribextremes
#export EXECPREP=${HOMEPREP}/exec
export EXECPREP=$GFDPTDIR/exec
#export FIXPREP=${HOMEPREP}/fix
#export FIXgempak=${HOMEPREP}/fix
export FIXgempak=${HOMESCRIPTS}/fix
#export HOMEDATA=${HOMEPREP}/data
#####################################################################
# Set up the UTILITIES
#####################################################################
export ushscript=/nw${envir}/ush
#export utilscript=/nwprod/util/ush
#export utilities=/nwprod/util/ush
export utilscript=/u/Andrew.Eichmann/nwprod/util/ush
export utilities=/u/Andrew.Eichmann/nwprod/util/ush
export utilexec=/nwprod/util/exec

#####################################################################
# Run setup to initialize working directory and utility scripts
#####################################################################
#sh $utilscript/setup.sh
export USHutil=/u/Andrew.Eichmann/nwprod/util/ush
sh $utilscript/setup.sh
#####################################################################

#####################################################################
# Run setpdy and initialize PDY variables
#####################################################################
#sh $utilscript/setpdy.sh
#. ./PDY
export PDY=$spdy
#export GIFOUT=/ptmpp2/$LOGNAME/fcstdiv/gfs_ecmwf_gribextremes_$PDY$cyc
export GIFOUT=/gpfs/hps2/ptmp/Andrew.Eichmann/fcstdiv/gfs_ecmwf_gribextremes_$PDY$cyc
mkdir -p $GIFOUT
#
   export pdy=$PDY
   export cpdy=$pdy$cyc
   export gcpdy=`echo $cpdy | cut -c3-8`
###   export mmdd=`date +%m%d`
   export mmdd=`echo $pdy | cut -c5-8`
   export fcyc=${cyc}00
#   export pcpdy=`/nwprod/util/exec/ndate -6 $cpdy`
   export pcpdy=`$NDATE -6 $cpdy`
   export ppdy=`echo $pcpdy | cut -c1-8`
   export pcyc=`echo $pcpdy | cut -c9-10`
   export year=`echo $pdy | cut -c1-4`

   export tcvital_file=/com/arch/prod/syndat/syndat_tcvitals.$year
   echo "pdy cyc year $pdy $cyc $year"
   grep "$pdy $cyc" $tcvital_file | sort | cut -c 20-44,63-66 | uniq > $gribout/tcv.$pdy$cyc
   size=`ls -l $gribout/tcv.$pdy$cyc | awk '{print $5}'`
   echo "Size $size"

   if [ $size -eq 0 ] ; then
      echo "$pdy $cyc   NONE" > $gribout/tcv.$pdy$cyc
   fi
#
#   export grib_gfsa=/com2/gfs/prod/gfs.$pdy
#   export grib_gfsg=/com2/gfs/prod/gdas.$ppdy
#   export grib_gfsa=/gpfs/hps/nco/ops/com/gfs/prod/gfs.$pdy
#   export grib_gfsg=/gpfs/hps/nco/ops/com/gfs/prod/gdas.$ppdy
#   export grib_gfsa=/global/noscrub/Fanglin.Yang/stat/gfs/
#   export grib_gfsg=/global/noscrub/Fanglin.Yang/stat/gfs/
   export grib_gfsa=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat/gfs/
   export grib_gfsg=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat/gfs/
#   export grib_ecma=/global/noscrub/emc.glopara/global/ecm  # EMC Location
#   export grib_ecma=/dcom/us007003/$pdy/wgrbbul/ecmwf
   export grib_ecma=//gpfs/dell1/nco/ops/dcom/prod/$pdy/wgrbbul/ecmwf/
   export ecmhead=DCD

   export idim=360
   export jdim=181

   export nlev=14
   export igau=0
#   export input_ges=$grib_gfsg/gdas1.t${pcyc}z.pgrbf06
#   export input_ges=$grib_gfsg/gdas.t${pcyc}z.pgrbf06
#   export input_gfs=$grib_gfsa/gfs.t${cyc}z.pgrbanl
   export input_ges=$grib_gfsg/pgbf06.gdas.${ppdy}${pcyc}
   export input_gfs=$grib_gfsa/pgbanl.gfs.${pdy}${cyc}


   export input_ecmwf=$grib_ecma/${ecmhead}$mmdd$fcyc$mmdd${cyc}001
   export tropl=$gribout/tcv.$pdy$cyc
   ln -sf  "$gribout/gemout.$pdy$cyc"                     "fort.55"
   ln -sf  "$gribout/gradsout.$pdy$cyc"                   "fort.60"
   ln -sf  "$gribout/gribext.trops.$pdy$cyc"               "fort.65"
   ln -sf  "$gribout/gribext.nontrops.$pdy$cyc"            "fort.66"
##   $EXECPREP/gribextremes.gfs.ecmwf.x > $gribout/rdgrib.gfs.ec.ges.$pdy$cyc <$tropl 2> $gribout/rdgrib_outout.$run.$pdy$cyc
   $EXECPREP/gfsecmwfgrib.x > $gribout/rdgrib.gfs.ec.ges.$pdy$cyc <$tropl 2> $gribout/rdgrib_outout.$pdy$cyc
#
   if [ -s $gribout/gribext.trops.$pdy$cyc ] ; then
      cat $gribout/gribext.trops.$pdy$cyc >> $gribout/gribhist.gfs.ecmwf.trops
   fi
#
   if [ -s $gribout/gribext.nontrops.$pdy$cyc ] ; then
      cat $gribout/gribext.nontrops.$pdy$cyc >> $gribout/gribhist.gfs.ecmwf.nontrops
   fi
#
# Call all the plotting Grads script
#
while read ijob
do
  p1=` echo $ijob | cut -d" " -f1`
  p2=` echo $ijob | cut -d" " -f2`
  p3=` echo $ijob | cut -d" " -f3`
  p4=` echo $ijob | cut -d" " -f4`
  p5=` echo $ijob | cut -d" " -f5`
  if [ $p1 == 'Z' ]; then
    $HOMESCRIPTS/wind.ecmwfgfs.setup $cpdy $p3 b 06 grdprs $p4 $p5 $p2
    echo  sleep 10  
  fi

  if [ $p1 == 'W' ]; then
    $HOMESCRIPTS/wind.ecmwfgfs.setup $cpdy $p3 b 06 grdprs $p4 $p5 $p2
    echo  sleep 10 
  fi

  if [ $p1 == 'T' ]; then
    $HOMESCRIPTS/temp.ecmwfgfs.setup $cpdy $p3 b 06 grdprs $p4 $p5 $p2
    sleep 10  
  fi

done < $gribout/gradsout.$pdy$cyc
#
# Run the GEMPAK script after executing the GRIB extremes script
#
sh $HOMESCRIPTS/gfs_ecmwf_gribextremes_mainpage.sh gemout.$pdy$cyc
exit
