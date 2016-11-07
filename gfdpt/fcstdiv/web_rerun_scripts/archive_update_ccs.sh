#!/bin/sh

PDY=`date "+%Y%m%d"`
echo $PDY

archPDY=`sh /nwprod/util/ush/finddate.sh $PDY d-31`
echo $archPDY

ssh -l sdm ncointra.ncep.noaa.gov "sh /home/nco/sdm/sdm/fcstdiv/main/archive_update_intrat.sh $PDY $archPDY"
