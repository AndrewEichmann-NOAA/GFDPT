#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`
export HH=00

export WEBDIR=/global/save/Andrew.Eichmann/r2o/fcstdiv/website/main


cd $WEBDIR/extdiff/$PDY$HH
for file in `ls *gif`
do
   convert $file  -resize 30% thumbnail.$file
done



