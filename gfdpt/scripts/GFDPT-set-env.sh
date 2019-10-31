#!/bin/sh
set -x

export PRODDEV=dev


export spdy=`date +%Y%m%d`
#export spdy=20180211

export GFDPTDIR=/global/save/Andrew.Eichmann/GFDPT/$PRODDEV
export GFDPTSCRIPTDIR=$GFDPTDIR/scripts

export GFDPTDATADIR=/global/noscrub/Andrew.Eichmann/GFDPT/$PRODDEV
export FFCORRDIR=$GFDPTDATADIR/ffcorr
export GFDPTTMPDIR=$GFDPTDATADIR/tmp

