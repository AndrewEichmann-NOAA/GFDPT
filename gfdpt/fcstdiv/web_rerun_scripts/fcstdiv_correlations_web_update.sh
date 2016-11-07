#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`
export HH=00

export INDIR=/nco/pmb/wx12ss/fcstdiv
export OUTDIR=/ptmp/wx12ss/fcstdiv/webout
export WEBDIR=/home/nco/sdm/sdm/fcstdiv/main

## PROGRAM SUBSTITUTION FOR DATE.TXT

sed -e "s/YYYYMMDDHH/${PDY}${HH}/g" $INDIR/template_date.txt > $OUTDIR/date.txt
scp $OUTDIR/date.txt sdm@ncointra:$WEBDIR/.

## PROGRAM SUBSTITUTIONS FOR INTRO_TAB.PHP

export fcstcor_nh=`cat /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/HGT/cor.day5.1.HGT.P500.G2NHX.PRA.txt`
export fcstcor_sh=`cat /stmp/wx12ss/f-f-divergence/vsdb_stats/anom/pra/HGT/cor.day5.1.HGT.P500.G2SHX.PRA.txt`
fcstcor_threshold=0.7

if [ $fcstcor_nh > $fcstcor_threshold ];
then
   export COLOR_NH=CHOCOLATE
else
   export COLOR_NH=RED
fi

if [ $fcstcor_sh > $fcstcor_threshold ];
then
   export COLOR_SH=CHOCOLATE
else
   export COLOR_SH=RED
fi

if [ $COLOR_NH = CHOCOLATE ];
then
   export PRIORITY_NH=LOW
else
   export PRIORITY_NH=HIGH
fi

if [ $COLOR_SH = CHOCOLATE ];
then
   export PRIORITY_SH=LOW
else
   export PRIORITY_SH=HIGH
fi

sed -e "s/\"COLOR_NH\"/\"$COLOR_NH\"/g" $INDIR/template_intro_tab.php > $OUTDIR/intro_tab1.php
sed -e "s/\"COLOR_SH\"/\"$COLOR_SH\"/g" $OUTDIR/intro_tab1.php > $OUTDIR/intro_tab2.php
sed -e "s/>PRIORITY_NH</>$PRIORITY_NH</g" $OUTDIR/intro_tab2.php > $OUTDIR/intro_tab3.php
sed -e "s/>PRIORITY_SH</>$PRIORITY_SH</g" $OUTDIR/intro_tab3.php > $OUTDIR/intro_tab4.php
sed -e "s/fcst_corr.YYYYMMDD/fcst_corr.$PDY/g" $OUTDIR/intro_tab4.php > $OUTDIR/intro_tab5.php

scp $OUTDIR/intro_tab5.php sdm@ncointra:$WEBDIR/intro_tab.php

## PROGRAM SUBSTITUTIONS FOR FCSTCOR_TAB.PHP and HTML CHART FILES

sed -e "s/fcst_corr.YYYYMMDD/fcst_corr.$PDY/g" $INDIR/template_fcstcor_tab.php > $OUTDIR/fcstcor_tab.php

scp $OUTDIR/fcstcor_tab.php sdm@ncointra:$WEBDIR/fcstcor_tab.php

#ls $INDIR/charts/template* > $OUTDIR/chartlist.txt
#newfilename=`grep template $OUTDIR/chartlist.txt | sed -e 's/\/nco\/pmb\/wx12ss\/fcstdiv\/charts\/template_//g' > $OUTDIR/newfilename.txt`
#cp $INDIR/charts/template* > $OUTDIR/*
#for file in $OUTDIR/*; do mv "$file" "${file#template_-}"; done

#for file in `ls $INDIR/charts/template*`
#do
#   cp $file $OUTDIR/$newfilename
#done


cp $INDIR/charts/template* $OUTDIR/.
cd $OUTDIR
for f in template_*; do mv "$f" "${f#template_}";done


for file in `ls $OUTDIR/*.html`
do
   sed "s/fcst_corr.YYYYMMDD/fcst_corr.$PDY/g" $file > $file.new
   mv $file.new $file
done

scp $OUTDIR/*.html sdm@ncointra:$WEBDIR/charts/.
