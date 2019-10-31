#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`
export HH=00

export WEBDIR=/global/save/Andrew.Eichmann/r2o/fcstdiv/website/main


cd $WEBDIR/extdiff/$PDY$HH
#for file in `ls *gif`
for file in `ls gfs*gif ecmwf*gif`
do
   convert $file  -resize 30% thumbnail.$file
done




cd $WEBDIR/ffplots/$PDY
for file in `ls fcst*png`
do
   convert $file  -resize 30% thumbnail.$file
done

convert fcstcor_HGT_P500_NH_day5.png -resize 70%  thumbnail70.fcstcor_HGT_P500_NH_day5.png
convert fcstcor_HGT_P500_SH_day5.png -resize 70%  thumbnail70.fcstcor_HGT_P500_SH_day5.png


convert fcstrms_HGT_P500_NH_day5.png -resize 70%  thumbnail70.fcstrms_HGT_P500_NH_day5.png
convert fcstrms_HGT_P500_SH_day5.png -resize 70%  thumbnail70.fcstrms_HGT_P500_SH_day5.png







