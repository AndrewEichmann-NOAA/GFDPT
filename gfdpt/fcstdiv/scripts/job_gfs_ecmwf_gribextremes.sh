#!/bin/sh
set -x
#####################################################################
echo "------------------------------------------------"
echo "job_gfs_ecmwf_gribextremes.sh"
echo "------------------------------------------------"
echo "V. Krishna Kumar Systems Integration Branch"
echo "History: APRIL 2011 - First implementation of this new script."
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
export gribout=/stmp/$LOGNAME/f-f-divergence/gribextremes_${NET}_${RUNM}_${pid}_${cyc}
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
export HOMEPREP=/nco/$grp/f-f-divergence
export HOMESCRIPTS=$HOMEPREP/scripts
export EXECPREP=${HOMEPREP}/exec
export FIXPREP=${HOMEPREP}/fix
export FIXgempak=${HOMEPREP}/fix
export HOMEDATA=${HOMEPREP}/data
#####################################################################
# Set up the UTILITIES
#####################################################################
export ushscript=/nw${envir}/ush
export utilscript=/nwprod/util/ush
export utilities=/nwprod/util/ush
export utilexec=/nwprod/util/exec

#####################################################################
# Run setup to initialize working directory and utility scripts
#####################################################################
sh $utilscript/setup.sh
#####################################################################

#####################################################################
# Run setpdy and initialize PDY variables
#####################################################################
sh $utilscript/setpdy.sh
. ./PDY
export GIFOUT=/ptmp/$LOGNAME/f-f-divergence/gfs_ecmwf_gribextremes_$PDY$cyc
mkdir -p $GIFOUT
#
   export pdy=$PDY
   export cpdy=$pdy$cyc
   export gcpdy=`echo $cpdy | cut -c3-8`
   export mmdd=`date +%m%d`
   export fcyc=${cyc}00
   export pcpdy=`/nwprod/util/exec/ndate -6 $cpdy`
   export ppdy=`echo $pcpdy | cut -c1-8`
   export pcyc=`echo $pcpdy | cut -c9-10`

   export grib_gfsa=/com/gfs/prod/gfs.$pdy
   export grib_gfsg=/com/gfs/prod/gdas.$ppdy
#   export grib_ecma=/global/shared/stat/ecm   EMC Location
   export grib_ecma=/dcom/us007003/$pdy/wgrbbul/ecmwf
   export ecmhead=ecens_DCD

   export idim=360
   export jdim=181

   export nlev=14
   export igau=0
   export input_ges=$grib_gfsg/gdas1.t${pcyc}z.pgrbf06
   export input_gfs=$grib_gfsa/gfs.t${cyc}z.pgrbanl
   export input_ecmwf=$grib_ecma/${ecmhead}$mmdd$fcyc$mmdd${cyc}001
   ln -sf  "$gribout/gemout.$pdy$cyc"                     "fort.55"
   ln -sf  "$gribout/gradsout.$pdy$cyc"                   "fort.60"
   $EXECPREP/gribextremes.gfs.ecmwf.x > $gribout/rdgrib.gfs.ec.ges.$pdy$cyc 2> $gribout/rdgrib_outout
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
