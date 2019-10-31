# Krishna Kumar NCO 01-7-2007.
set -x
#. /u/$LOGNAME/.profile
date
export envir=prod
export cyc=00
export NET=gfs
export RUNM=ecmwf
export job=${NET}_${RUNM}_gribextremes_${cyc}
export grp=save
# CALL executable job script here
#sh /global/$grp/$LOGNAME/r2o/fcstdiv/scripts/job_gfs_ecmwf_gribextremes.sh
sh $GFDPTSCRIPTDIR/gribextremes/job_gfs_ecmwf_gribextremes.sh
