#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`
export HH=00


export WEBSITEDIR=/global/save/Andrew.Eichmann/r2o/fcstdiv/website
export INDIR=$WEBSITEDIR/templates
export OUTDIR=$WEBSITEDIR/webout
export WEBDIR=$WEBSITEDIR/main
export FFPLOTDIR=$WEBSITEDIR/main/ffplots/$PDY

## PROGRAM SUBSTITUTION FOR DATE.TXT

sed -e "s/YYYYMMDDHH/${PDY}${HH}/g" $INDIR/template_date.txt > $OUTDIR/date.txt
#scp $OUTDIR/date.txt sdm@ncointra:$WEBDIR/.
cp $OUTDIR/date.txt $WEBDIR

export COR_ALERT_NH=`cat $FFPLOTDIR/COR_ALERT_NH.txt`
export COR_ALERT_SH=`cat $FFPLOTDIR/COR_ALERT_SH.txt`
export RMS_ALERT_NH=`cat $FFPLOTDIR/RMS_ALERT_NH.txt`
export RMS_ALERT_SH=`cat $FFPLOTDIR/RMS_ALERT_SH.txt`

if [ $COR_ALERT_NH = NONE ];
then
   export COLOR_NH_COR=BLACK
else
   export COLOR_NH_COR=$COR_ALERT_NH
fi

if [ $COR_ALERT_SH = NONE ];
then
   export COLOR_SH_COR=BLACK
else
   export COLOR_SH_COR=$COR_ALERT_SH
fi

if [ $RMS_ALERT_NH = NONE ];
then
   export COLOR_NH_RMS=BLACK
else
   export COLOR_NH_RMS=$RMS_ALERT_NH
fi

if [ $RMS_ALERT_SH = NONE ];
then
   export COLOR_SH_RMS=BLACK
else
   export COLOR_SH_RMS=$RMS_ALERT_SH
fi


sed -e "s/\"COLOR_NH_COR\"/\"$COLOR_NH_COR\"/g" $INDIR/template_fcstcor_tab.php > $OUTDIR/fcstcor_tab1.php
sed -e "s/\"COLOR_SH_COR\"/\"$COLOR_SH_COR\"/g" $OUTDIR/fcstcor_tab1.php > $OUTDIR/fcstcor_tab2.php
sed -e "s/>COR_ALERT_NH</>$COR_ALERT_NH</g" $OUTDIR/fcstcor_tab2.php > $OUTDIR/fcstcor_tab3.php
sed -e "s/>COR_ALERT_SH</>$COR_ALERT_SH</g" $OUTDIR/fcstcor_tab3.php > $OUTDIR/fcstcor_tab4.php
sed -e "s/YYYYMMDD/$PDY/g" $OUTDIR/fcstcor_tab4.php > $OUTDIR/fcstcor_tab5.php

cp $OUTDIR/fcstcor_tab5.php $WEBDIR/fcstcor_tab.php

sed -e "s/\"COLOR_NH_RMS\"/\"$COLOR_NH_RMS\"/g" $INDIR/template_fcstrms_tab.php > $OUTDIR/fcstrms_tab1.php
sed -e "s/\"COLOR_SH_RMS\"/\"$COLOR_SH_RMS\"/g" $OUTDIR/fcstrms_tab1.php > $OUTDIR/fcstrms_tab2.php
sed -e "s/>RMS_ALERT_NH</>$RMS_ALERT_NH</g" $OUTDIR/fcstrms_tab2.php > $OUTDIR/fcstrms_tab3.php
sed -e "s/>RMS_ALERT_SH</>$RMS_ALERT_SH</g" $OUTDIR/fcstrms_tab3.php > $OUTDIR/fcstrms_tab4.php
sed -e "s/YYYYMMDD/$PDY/g" $OUTDIR/fcstrms_tab4.php > $OUTDIR/fcstrms_tab5.php

cp $OUTDIR/fcstrms_tab5.php $WEBDIR/fcstrms_tab.php





#ls $INDIR/charts/template* > $OUTDIR/chartlist.txt
#newfilename=`grep template $OUTDIR/chartlist.txt | sed -e 's/\/nco\/pmb\/wx12ss\/fcstdiv\/charts\/template_//g' > $OUTDIR/newfilename.txt`
#cp $INDIR/charts/template* > $OUTDIR/*
#for file in $OUTDIR/*; do mv "$file" "${file#template_-}"; done

#for file in `ls $INDIR/charts/template*`
#do
#   cp $file $OUTDIR/$newfilename
#done








#cp $INDIR/charts/template* $OUTDIR/.
#cd $OUTDIR
#for f in template_*; do mv "$f" "${f#template_}";done


#for file in `ls $OUTDIR/*.html`
#do
#   sed "s/fcst_corr.YYYYMMDD/fcst_corr.$PDY/g" $file > $file.new
#   mv $file.new $file
#done

#scp $OUTDIR/*.html sdm@ncointra:$WEBDIR/charts/.
#cp $OUTDIR/*.html $WEBDIR/charts/.
