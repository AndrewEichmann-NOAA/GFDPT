#!/bin/sh
set -x
#
# V. Krishna Kumar/NCO/SIB May 2011
#  Check if the 5-day (00Z) or 4-day (12Z)forecast (GFS) - forecast (ECMWF) correlations drop below 0.7 
#  in the Northern and Southern Hemispheres to alert the SDMs, Dropout team and other interested parties
#
#
#cor_files=`ls cor*txt`
export rundir=/stmp/$LOGNAME/f-f-divergence/vsdb_stats
cd $rundir/anom/pra/HGT
cor_files=`ls cor*txt`
for file in $cor_files
do

int_lmt=`echo $cor_val '*10000000' | bc -l | awk -F '.' '{ print $1; exit; }'`
                      # This is how much accuracy we have
                      # (to the nearest 10 millionth for this example)
                      #
limit=0.7
#
# Convert big decimal into a big integer for comparing
#
ilimit=`echo $limit '*10000000' | bc -l | awk -F '.' '{ print $1; exit; }'`
                      #
if [ $file = cor.day5.1.HGT.P500.G2NHX.PRA.txt ];then
   cor_val=`cat $file`
   icor_val=`echo $cor_val '*10000000' | bc -l | awk -F '.' '{ print $1; exit; }'`
   if [ "$cor_val" -eq -99.9 ]; then
      echo "Undefined correlation value"
   elif [ $icor_val -lt $ilimit ]; then
   cyc=00
   cpdy=`date +%Y%m%d`
   fpdy=`/nwprod/util/ush/finddate.sh $cpdy d+5`
   echo " " > sdm_email1
   echo " " >> sdm_email1
   echo " " >> sdm_email1
   echo " " >> sdm_email1
   echo "The 5-day Forecast-Forecast correlation anomalies for the Northern Hemisphere indicate" >> sdm_email1
   echo "the potential for a forecast skill dropout to occur." >> sdm_email1 
   echo " " >> sdm_email1
   echo " " >> sdm_email1
   echo "Please go to the following site for more information and graphics:" >> sdm_email1
   echo "http://www2.nco.ncep.noaa.gov/pmb/docs/sdm/fcstdiv" >> sdm_email1
   echo " " >> sdm_email1
   echo " " >> sdm_email1
   cat sdm_email1 | mail -s "Forecast Divergence High Alert - Northern Hemisphere" "ncep.list.dropout-forecast@noaa.gov"
   fi
###
###
###
elif [ $file = cor.day5.1.HGT.P500.G2SHX.PRA.txt ];then
   cor_val=`cat $file`
   icor_val=`echo $cor_val '*10000000' | bc -l | awk -F '.' '{ print $1; exit; }'`
   if [ "$cor_val" -eq -99.9 ]; then
      echo "Undefined correlation value"
   elif [ $icor_val -lt $ilimit ]; then
   cyc=00
   cpdy=`date +%Y%m%d`
   fpdy=`/nwprod/util/ush/finddate.sh $cpdy d+5`
   echo " " > sdm_email2
   echo " " >> sdm_email2
   echo " " >> sdm_email2
   echo " " >> sdm_email2
   echo "The 5-day Forecast-Forecast correlation anomalies for the Southern Hemisphere indicate" >> sdm_email2
   echo "the potential for a forecast skill dropout to occur." >> sdm_email2 
   echo " " >> sdm_email2
   echo " " >> sdm_email2
   echo "Please go to the following site for more information and graphics:" >> sdm_email2
   echo "http://www2.nco.ncep.noaa.gov/pmb/docs/sdm/fcstdiv" >> sdm_email2
   echo " " >> sdm_email2
   echo " " >> sdm_email2
   cat sdm_email2 | mail -s "Forecast Divergence High Alert - Southern Hemisphere" "ncep.list.dropout-forecast@noaa.gov"
   echo " " >> sdm_email2
   fi
###
###
###
elif [ $file = cor.day5.2.HGT.P500.G2NHX.PRA.txt ];then
   cor_val=`cat $file`
   icor_val=`echo $cor_val '*10000000' | bc -l | awk -F '.' '{ print $1; exit; }'`
   if [ "$cor_val" -eq -99.9 ]; then
      echo "Undefined correlation value"
   elif [ $icor_val -lt $ilimit ]; then
   cyc=12
   cpdy=`date +%Y%m%d`
   fpdy=`/nwprod/util/ush/finddate.sh $cpdy d+4`
   echo " " > sdm_email3
   echo " " >> sdm_email3
   echo " " >> sdm_email3
   echo " " >> sdm_email3
   echo "The 5-day Forecast-Forecast correlation anomalies for the Northern Hemisphere indicate" >> sdm_email3
   echo "the potential for a forecast skill dropout to occur." >> sdm_email3 
   echo " " >> sdm_email3
   echo " " >> sdm_email3
   echo "Please go to the following site for more information and graphics:" >> sdm_email3
   echo "http://www2.nco.ncep.noaa.gov/pmb/docs/sdm/fcstdiv" >> sdm_email3
   echo " " >> sdm_email3
   echo " " >> sdm_email3
   cat sdm_email3 | mail -s "Forecast Divergence High Alert - Northern Hemisphere" "ncep.list.dropout-forecast@noaa.gov"
   echo " " >> sdm_email3
   fi
###
###
###
elif [ $file = cor.day5.2.HGT.P500.G2SHX.PRA.txt ];then
   cor_val=`cat $file`
   icor_val=`echo $cor_val '*10000000' | bc -l | awk -F '.' '{ print $1; exit; }'`
   if [ "$cor_val" -eq -99.9 ]; then
      echo "Undefined correlation value"
   elif [ $icor_val -lt $ilimit ]; then
   cyc=12
   cpdy=`date +%Y%m%d`
   fpdy=`/nwprod/util/ush/finddate.sh $cpdy d+4`
   echo " " > sdm_email4
   echo " " >> sdm_email4
   echo " " >> sdm_email4
   echo " " >> sdm_email4
   echo "The 5-day Forecast-Forecast correlation anomalies for the Southern Hemisphere indicate" >> sdm_email4
   echo "the potential for a forecast skill dropout to occur." >> sdm_email4 
   echo " " >> sdm_email4
   echo " " >> sdm_email4
   echo "Please go to the following site for more information and graphics:" >> sdm_email4
   echo "http://www2.nco.ncep.noaa.gov/pmb/docs/sdm/fcstdiv" >> sdm_email4
   echo " " >> sdm_email4
   echo " " >> sdm_email4
   cat sdm_email4 | mail -s "Forecast Divergence High Alert - Southern Hemisphere" "ncep.list.dropout-forecast@noaa.gov"
   echo " " >> sdm_email4
   fi
###
###
###
fi
done
