#!/bin/sh

set -xa

PDY=$1
PRIORITY=$2
ID=$3

WKDIR=/home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/$PRIORITY/$ID
cd $WKDIR

giflist=`ls *.gif`

for giffile in $giflist
do
   convert -sample 125x125 $giffile thumbnail.$giffile
done
