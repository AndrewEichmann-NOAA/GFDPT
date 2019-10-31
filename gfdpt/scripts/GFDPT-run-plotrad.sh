#!/bin/sh
set -x

export cyc=00
export spdycyc=${spdy}${cyc}

export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/GFDPT-plotrad.sh &
sh $GFDPTSCRIPTDIR/GFDPT-clim.sh &


export spdycyc=`${NDATE} -18 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/GFDPT-plotrad.sh &
sh $GFDPTSCRIPTDIR/GFDPT-clim.sh &

#exit

export spdycyc=`${NDATE} 6 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/GFDPT-plotrad.sh &
sh $GFDPTSCRIPTDIR/GFDPT-clim.sh &


export spdycyc=`${NDATE} 6 ${spdycyc}`
export spdy=`echo $spdycyc | cut -c1-8`
export cyc=`echo $spdycyc | cut -c9-10`

sh $GFDPTSCRIPTDIR/GFDPT-plotrad.sh &
sh $GFDPTSCRIPTDIR/GFDPT-clim.sh &



