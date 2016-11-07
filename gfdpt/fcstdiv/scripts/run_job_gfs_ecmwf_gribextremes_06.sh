# Krishna Kumar NCO 01-7-2007.
set -x
. /u/$LOGNAME/.profile
date
export envir=prod
export cyc=06
export NET=gfs
export RUNM=ecmwf
export job=${NET}_${RUNM}_gribextremes_${cyc}
export grp=pmb

# CALL executable job script here
sh /nco/$grp/f-f-divergence/scripts/job_gfs_ecmwf_gribextremes.sh
