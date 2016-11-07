# Krishna Kumar NCO 01-7-2007.
set -x
. /u/$LOGNAME/.profile
date
export envir=prod
#
export grp=hwrf
#
#
# First run the 12z cycle for the previous day since the ECMWF GRIB1 files
# reach NCEP after a delay of 24 hours 
#
export cyc=12
export NET=gfs
export RUNV=ecmwf
export job=corr_${NET}_vs_${RUNV}_${cyc}
export runtmp=/stmpd2/$LOGNAME/f-f_correlation
#
export homedir=/sss/emc/hwrf/save/$LOGNAME/fcstdiv
#
mkdir -p $runtmp
export output=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
export error=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
#
# Save all the pgb GRIB1 files of GFS and ECMWF analysis/forecast files
# Change the forecast dates (24, 48, 72, 96 and 120 hour) of ECMWF to the
# verifying analysis dates using the executable "overdate.grib" of GFS
# forecast hours
#
sh $homedir/scripts/copy_f-f_gfs_ecmwf.sh 
#
# Generate the VSDB stats files using the GFS pgb analysis/forecast files
# and the modified ECMWF pgb analysis/forecast files treating the ECMWF
# forecasts files (24, 48, 72, 96 and 120 hours) as the respective verifying 
# analysis
#
sh $homedir/scripts/vsdbjob_submit.sh
#
# Next run the same scripts for 00z cycle for the current day since there is
# not a considerable delay in receiving the ECMWF GRIB1 files
#
export cyc=00
sh $homedir/scripts/copy_f-f_gfs_ecmwf.sh
sh $homedir/scripts/vsdbjob_submit.sh
#
# Finally plot the 5-day forecast-forecast correlation timeseries of GFS with 
# ECMWF of 00z and 12z cycles using the generated VSDB data files going back to 
# 36 days from the 5-day forecast date of current cycle (previous day cycle) 
# for 00z (12z)  
#
sh $homedir/scripts/job_corr_gfs_vs_ecmwf_plot_ncyc.sh

# Run the e-mail script
#
sh $homedir/scripts/vsdb_f-f_dalert.sh

exit
