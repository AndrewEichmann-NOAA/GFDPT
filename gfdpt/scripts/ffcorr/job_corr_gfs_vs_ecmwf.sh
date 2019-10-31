# Krishna Kumar NESDIS/JCSDA March 20, 2017
set -x
# . /u/$LOGNAME/.profile
date
export envir=prod
#
export grp=global
export NET=gfs
export RUNV=ecmwf
export runtmp=/gpfs/hps3/stmp/$LOGNAME/fcstdiv # XXX 
#
#
# First run the 12z cycle for the previous day since the ECMWF GRIB1 files
# reach NCEP after a delay of 24 hours 
#
#export cyc=12
export spdy=`echo $spdy12z | cut -c1-8`
export cyc=`echo $spdy12z | cut -c9-10`

export job=corr_${NET}_vs_${RUNV}_${cyc}
#
mkdir -p $runtmp
export output=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
export error=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
#
# Save all the pgb GRIB1 files of GFS and ECMWF analysis/forecast files
# Change the forecast dates (24, 48, 72, 96, 120, 144, 168, 192, 216 and 240 hour) of ECMWF to the
# verifying analysis dates using the executable "overdate.grib" of GFS
# forecast hours
#
sh $FFCORRSCRIPTDIR/copy_f-f_gfs_ecmwf.sh 

# Generate the VSDB stats files using the GFS pgb analysis/forecast files
# and the modified ECMWF pgb analysis/forecast files treating the ECMWF
# forecasts files (24, 48, 72, 96, 120, 144, 168, 192, 216 and 240 hour) as the respective verifying 
# analysis
#
sh $FFCORRSCRIPTDIR/vsdbjob_submit1.sh
#
# Next run the same scripts for 00z cycle for the current day since there is
# not a considerable delay in receiving the ECMWF GRIB1 files
#
#export cyc=00
export spdy=`echo $spdy00z | cut -c1-8`
export cyc=`echo $spdy00z | cut -c9-10`

export job=corr_${NET}_vs_${RUNV}_${cyc}
#
#
mkdir -p $runtmp
export output=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
export error=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}

sh $FFCORRSCRIPTDIR/copy_f-f_gfs_ecmwf.sh
sh $FFCORRSCRIPTDIR/vsdbjob_submit1.sh
#
exit
