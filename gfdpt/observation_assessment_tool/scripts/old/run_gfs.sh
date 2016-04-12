#!/bin/bash 
#########################################################
# Description:
#    Decode SSMIS BUFR to obs FMSDR
#    Colocate GFS forecast data to obs FMSDR
#    Forward model on colocated GFS to create simulated FMSDR
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Date: 5/12/2014
# Usage: ./run_gfs.sh   f16
#        ./run_gfs.sh   f17
#        ./run_gfs.sh   f18
#
#
#########################################################
sensor=$1
# default to f18

EDR_COLOC_NWP_DIR='edr_coloc_nwp'
CONF_DIR='config'
EXE1='./main_read_ssmis.exe'
EXE2='./colocNWPwRad'
EXE3='./fwd'

# GFS  BUFR: 1 / day at 00Z
#-----------------------------------------
# Give dates (user-specified)
#-----------------------------------------
dates="2013060700 2013060800 2013060900 "
for date in $dates
do 
   main_read_ssmis_config=${CONF_DIR}/ssmisu_GFS_${date}_${sensor}.config
   ${EXE1} < ${main_read_ssmis_config}
   echo "done with main_read : $date"

   coloc_nwp_config=${CONF_DIR}/colocNWP_GFS_${date}_${sensor}.config
   ${EXE2} < ${coloc_nwp_config}
   echo "done with coloc nwp : $date"

   fwd_config=${CONF_DIR}/fwd_GFS_${date}_${sensor}.config
   ${EXE3} < ${fwd_config}
   echo "done with fwd : $date"

   # Remove edr file to save space
   rm ${EDR_COLOC_NWP_DIR}/NWP* 

done
