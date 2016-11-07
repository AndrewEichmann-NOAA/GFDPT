#!/bin/sh

PDY=$1

WKDIR=/home/nco/sdm/sdm/fcstdiv/main/correlations/fcst_corr.$PDY
cd $WKDIR

pnglist=`ls *.png`

for pngfile in $pnglist
do
   convert -sample 125x125 $pngfile thumbnail.$pngfile
done

pnglist500=`ls cor_day5_*_HGT_P500_*.png`

for pngfile500 in $pnglist500
do
   convert -sample 380x380 $pngfile500 thumbnail500.$pngfile500
done

