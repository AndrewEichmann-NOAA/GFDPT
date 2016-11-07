#!/bin/sh

set -xa

export PDY=`date "+%Y%m%d"`00

export INDIR=/nco/pmb/wx12ss/fcstdiv
export DATADIR=/ptmp/wx12ss/f-f-divergence/gfs_ecmwf_gribextremes_$PDY
export OUTDIR=/ptmp/wx12ss/fcstdiv/webout
export WEBDIR=/home/nco/sdm/sdm/fcstdiv/main

rm $OUTDIR/*
ssh -l sdm ncointra "rm $WEBDIR/case_high* $WEBDIR/case_low*"

## PROGRAM SUBSTITUTIONS FOR EXTANLDIFF_TAB.PHP

## While loop to create the needed files for each individual case.

export casefile=$DATADIR/gemout.$PDY

ls $DATADIR/*_$PDY*.gif | sed -e "s/\/ptmp\/wx12ss\/f-f-divergence\/gfs_ecmwf_gribextremes_$PDY\///g" -e "s/_hgt_/ hgt /g" -e "s/_tmp_/ tmp /g" -e "s/.gif/ gif/g" -e "s/${PDY}_/${PDY} /g" > $OUTDIR/giflist

exec 7<$casefile
while read -u7 line; do
   echo $line > $OUTDIR/casefile
   caseid=`awk '{printf "%4s_%3s_%4s", $2, $3, $4 }' $OUTDIR/casefile > $OUTDIR/caseid.out`
   cat $OUTDIR/caseid.out
   sed -e 's/ //g' $OUTDIR/caseid.out > $OUTDIR/caseid
   id1=`cat $OUTDIR/caseid`
   id2=`cat $OUTDIR/caseid | sed -e 's/_/ /g'` 
   pmeter=`cat $OUTDIR/caseid | cut -c1`
   if [[ $pmeter = "Z" ]]
   then
      parameter=Height
   elif [[ $pmeter = "T" ]]
   then
      parameter=Temperature
   else
      parameter=Wind
   fi
   echo $id2 > $OUTDIR/caseid2.out
   id=`awk '{ print $1 }' $OUTDIR/caseid2.out` 
   lat=`awk '{ print $2 }' $OUTDIR/caseid2.out`
   lon=`awk '{ print $3 }' $OUTDIR/caseid2.out`
 
   priority=`awk '{ print $8 }' $OUTDIR/casefile`
   casenumber=`awk '{ print $1 }' $OUTDIR/casefile`

   if [ $priority = 1 ]
   then 
      priorityval=high
      echo $id1 >> $OUTDIR/caselist_high.out
      cp $INDIR/template_case_high_caseid.php $OUTDIR/case_high_$id1.php.out1
      sed -e "s/YYYYMMDDHH/$PDY/g" -e "s/PRIORITY/$priorityval/g" -e "s/CASEID/$id1/g" -e "s/CASE/$id($lat,$lon)/g" -e "s/LATI/$lat/g" -e "s/LON/$lon/g" -e "s/PARAMETER/$parameter/g"  $OUTDIR/case_high_$id1.php.out1 > $OUTDIR/case_high_$id1.php.out2
      for gifcase in `awk '{ print $4 }' $OUTDIR/giflist`
      do
         if [[ "${gifcase%%_*}" == "$casenumber" ]]
         then
            plvl=`grep $gifcase $OUTDIR/giflist | awk '{ print $2 }' | uniq`
            param=`grep $gifcase $OUTDIR/giflist | awk '{ print $3 }' | uniq`
            x_y_z=`grep $gifcase $OUTDIR/giflist | awk '{ print $4 }' | uniq`
            sed -e "s/PRESSURE/$plvl/g" $OUTDIR/case_high_$id1.php.out2 > $OUTDIR/case_high_$id1.php.out3
            sed -e "s/PARAM/$param/g" $OUTDIR/case_high_$id1.php.out3 > $OUTDIR/case_high_$id1.php.out4
            sed -e "s/X_Y_Z/${x_y_z}/g" $OUTDIR/case_high_$id1.php.out4 > $OUTDIR/case_high_$id1.php
            scp $OUTDIR/case_high_$id1.php sdm@ncointra:$WEBDIR
         fi
      done
   else
      priorityval=low
      echo $id1 >> $OUTDIR/caselist_low.out
      cp $INDIR/template_case_low_caseid.php $OUTDIR/case_low_$id1.php.out1
      sed -e "s/YYYYMMDDHH/$PDY/g" -e "s/PRIORITY/$priorityval/g" -e "s/CASEID/$id1/g" -e "s/CASE/$id($lat,$lon)/g" -e "s/LATI/$lat/g" -e "s/LON/$lon/g" -e "s/PARAMETER/$parameter/g"  $OUTDIR/case_low_$id1.php.out1 > $OUTDIR/case_low_$id1.php.out2
      for gifcase in `awk '{ print $4 }' $OUTDIR/giflist`
      do
         if [[ "${gifcase%%_*}" == "$casenumber" ]]
         then
            plvl=`grep $gifcase $OUTDIR/giflist | awk '{ print $2 }' | uniq`
            param=`grep $gifcase $OUTDIR/giflist | awk '{ print $3 }' | uniq`
            x_y_z=`grep $gifcase $OUTDIR/giflist | awk '{ print $4 }' | uniq`
            sed -e "s/PRESSURE/$plvl/g" $OUTDIR/case_low_$id1.php.out2 > $OUTDIR/case_low_$id1.php.out3
            sed -e "s/PARAM/$param/g" $OUTDIR/case_low_$id1.php.out3 > $OUTDIR/case_low_$id1.php.out4
            sed -e "s/X_Y_Z/${x_y_z}/g" $OUTDIR/case_low_$id1.php.out4 > $OUTDIR/case_low_$id1.php
            scp $OUTDIR/case_low_$id1.php sdm@ncointra:$WEBDIR
         fi
      done
   fi
done

sed -e "s/ext_anl.YYYYMMDDHH/ext_anl.$PDY/g" -e "s/gemout.YYYYMMDDHH/gemout.$PDY/g" $INDIR/template_extanldiff_tab.php > $OUTDIR/extanldiff_tab1.php

highnum=`cat $OUTDIR/caselist_high.out | wc -l`
lownum=`cat $OUTDIR/caselist_low.out | wc -l`
highline1=28
highline2=`expr $highline1 + $highnum - 1`
lowline1=64
lowline2=`expr $lowline1 + $lownum - 1`
sed "${highline1},${highline2}s/\/\/ //" $OUTDIR/extanldiff_tab1.php > $OUTDIR/extanldiff_tab2.php
sed "${lowline1},${lowline2}s/\/\/ //" $OUTDIR/extanldiff_tab2.php > $OUTDIR/extanldiff_tab3.php

linestart=$highline1
tabstart=3
for id3 in `cat $OUTDIR/caselist_high.out`
do
   newtab=`expr $tabstart + 1`
   echo $id3 > $OUTDIR/$id3.high.out
   sed "${linestart}s/CASEID/${id3}/" $OUTDIR/extanldiff_tab${tabstart}.php > $OUTDIR/extanldiff_tab${newtab}.php
   linestart=`expr $linestart + 1`
   tabstart=`expr $tabstart + 1`
done

linestart=$lowline1
tabstart=$newtab
for id4 in `cat $OUTDIR/caselist_low.out`
do
   newtab=`expr $tabstart + 1`
   echo $id4 > $OUTDIR/$id4.low.out
   sed "${linestart}s/CASEID/${id4}/" $OUTDIR/extanldiff_tab${tabstart}.php > $OUTDIR/extanldiff_tab${newtab}.php
   cp $OUTDIR/extanldiff_tab${newtab}.php $OUTDIR/extanldiff_tab.php
   linestart=`expr $linestart + 1`
   tabstart=`expr $tabstart + 1`
done

scp $OUTDIR/extanldiff_tab.php sdm@ncointra:$WEBDIR/.
