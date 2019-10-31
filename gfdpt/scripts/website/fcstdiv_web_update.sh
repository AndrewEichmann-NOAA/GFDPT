#!/bin/sh

set -xa
export DATEM5D=`date -d'5 days ago' +%Y%m%d`
export HH=00

export FFPLOTDIR=$WEBSTAGEDIR/ffcorr/$spdy

## PROGRAM SUBSTITUTION FOR DATE.TXT

sed -e "s/YYYYMMDDHH/${spdy}${HH}/g" $TEMPLATESDIR/template_date.txt > $GFDPTTMPDIR/date.txt

cp $GFDPTTMPDIR/date.txt $WEBSTAGEDIR

sed -e "s/YYYYMMDDHH/${spdy}${HH}/g" $TEMPLATESDIR/template_plotrad_tab.php > $WEBSTAGEDIR/plotrad_tab.php
sed -e "s/YYYYMMDDHH/${spdy}${HH}/g" $TEMPLATESDIR/template_radclim_tab.php > $WEBSTAGEDIR/radclim_tab.php
sed -e "s/YYYYMMDDHH/${DATEM5D}${HH}/g" $TEMPLATESDIR/template_errorevo_tab.php > $WEBSTAGEDIR/errorevo_tab.php

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


sed -e "s/\"COLOR_NH_COR\"/\"$COLOR_NH_COR\"/g" $TEMPLATESDIR/template_fcstcor_tab.php > $GFDPTTMPDIR/fcstcor_tab1.php
sed -e "s/\"COLOR_SH_COR\"/\"$COLOR_SH_COR\"/g" $GFDPTTMPDIR/fcstcor_tab1.php > $GFDPTTMPDIR/fcstcor_tab2.php
sed -e "s/>COR_ALERT_NH</>$COR_ALERT_NH</g" $GFDPTTMPDIR/fcstcor_tab2.php > $GFDPTTMPDIR/fcstcor_tab3.php
sed -e "s/>COR_ALERT_SH</>$COR_ALERT_SH</g" $GFDPTTMPDIR/fcstcor_tab3.php > $GFDPTTMPDIR/fcstcor_tab4.php
sed -e "s/YYYYMMDD/$spdy/g" $GFDPTTMPDIR/fcstcor_tab4.php > $GFDPTTMPDIR/fcstcor_tab5.php

cp $GFDPTTMPDIR/fcstcor_tab5.php $WEBSTAGEDIR/fcstcor_tab.php

sed -e "s/\"COLOR_NH_RMS\"/\"$COLOR_NH_RMS\"/g" $TEMPLATESDIR/template_fcstrms_tab.php > $GFDPTTMPDIR/fcstrms_tab1.php
sed -e "s/\"COLOR_SH_RMS\"/\"$COLOR_SH_RMS\"/g" $GFDPTTMPDIR/fcstrms_tab1.php > $GFDPTTMPDIR/fcstrms_tab2.php
sed -e "s/>RMS_ALERT_NH</>$RMS_ALERT_NH</g" $GFDPTTMPDIR/fcstrms_tab2.php > $GFDPTTMPDIR/fcstrms_tab3.php
sed -e "s/>RMS_ALERT_SH</>$RMS_ALERT_SH</g" $GFDPTTMPDIR/fcstrms_tab3.php > $GFDPTTMPDIR/fcstrms_tab4.php
sed -e "s/YYYYMMDD/$spdy/g" $GFDPTTMPDIR/fcstrms_tab4.php > $GFDPTTMPDIR/fcstrms_tab5.php

cp $GFDPTTMPDIR/fcstrms_tab5.php $WEBSTAGEDIR/fcstrms_tab.php


