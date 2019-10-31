#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`
export HH=00

#export INDIR=/nco/pmb/wx12ss/fcstdiv
export INDIR=/home/people/emc/www/htdocs/GFDPT/aeichmann/sandbox/templates
#export OUTDIR=/ptmp/wx12ss/fcstdiv/webout
export OUTDIR=/home/people/emc/www/htdocs/GFDPT/aeichmann/sandbox/webout
#export WEBDIR=/home/nco/sdm/sdm/fcstdiv/main
export WEBDIR=/home/people/emc/www/htdocs/GFDPT/aeichmann/sandbox/main


cd $WEBDIR/extdiff/$PDY$HH
for file in `ls *gif`
do
   convert $file  -resize 30% thumbnail.$file
done



