#!/bin/sh
set -x

export cyc=00
export spdycyc=${spdy}${cyc}

#export spdycyc=`/nwprod/util/exec/ndate -18 ${spdycyc}`
export spdycyc=`$NDATE -18 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`


sh $GFDPTSCRIPTDIR/gribextremes/run_job_gfs_ecmwf_gribextremes_${cyc}.sh

mkdir  $WEBSTAGEDIR/extdiff/${spdycyc}
#cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gfs_ecmwf_gribextremes_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gribextremes_gfs_ecmwf_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
sh $GFDPTSCRIPTDIR/GFDPT-gribext-makeweb.sh


#export spdycyc=`/nwprod/util/exec/ndate 6 ${spdycyc}`
export spdycyc=`$NDATE 6 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/gribextremes/run_job_gfs_ecmwf_gribextremes_${cyc}.sh

mkdir  $WEBSTAGEDIR/extdiff/${spdycyc}
#cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gfs_ecmwf_gribextremes_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gribextremes_gfs_ecmwf_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
sh $GFDPTSCRIPTDIR/GFDPT-gribext-makeweb.sh


#export spdycyc=`/nwprod/util/exec/ndate 6 ${spdycyc}`
export spdycyc=`$NDATE 6 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/gribextremes/run_job_gfs_ecmwf_gribextremes_${cyc}.sh

mkdir  $WEBSTAGEDIR/extdiff/${spdycyc}
#cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gfs_ecmwf_gribextremes_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gribextremes_gfs_ecmwf_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
sh $GFDPTSCRIPTDIR/GFDPT-gribext-makeweb.sh

#export spdycyc=`/nwprod/util/exec/ndate 6 ${spdycyc}`
export spdycyc=`$NDATE 6 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/gribextremes/run_job_gfs_ecmwf_gribextremes_${cyc}.sh

mkdir  $WEBSTAGEDIR/extdiff/${spdycyc}
#cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gfs_ecmwf_gribextremes_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
cp -r /gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/gribextremes_gfs_ecmwf_${spdycyc}/* $WEBSTAGEDIR/extdiff/${spdycyc}
sh $GFDPTSCRIPTDIR/GFDPT-gribext-makeweb.sh



