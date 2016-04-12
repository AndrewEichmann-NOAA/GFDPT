#!/bin/bash 
#########################################################
# Description:
#    Decode SSMIS BUFR to obs FMSDR
#    Colocate GFS forecast data to obs FMSDR
#    Forward model on colocated GFS to create simulated FMSDR
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Date: 5/12/2014
# Usage: ./run_gdas.sh   f16
#        ./run_gdas.sh   f17
#        ./run_gdas.sh   f18
#
#########################################################
if [ $# -ne 1 ]
then
   echo ""
   echo "sesnor ID needed!!!"
   echo "choices are : f16, f17 or f18 "
   echo ""
   echo ""
fi

# Specify dates you want to run.
dates="2013060700 2013060706 "

# default to f18
sensor=$1

EDR_COLOC_NWP_DIR='edr_coloc_nwp'
CONF_DIR='config'
EXE1='../../../bin/ssmisBUFR_Converter.exe'
EXE2='../../../bin/colocNWPwRad'
EXE3='../../../bin/fwd'

# GDAS BUFR : 4 / day at 00Z, 06Z, 12Z and 18Z
#-----------------------------------------
# Give dates (user-specified) 
#-----------------------------------------
for date in $dates
do 
   ssmisBUFR_Converter_config=${CONF_DIR}/ssmisu_gdas_${date}_${sensor}.config
   ${EXE1} < ${ssmisBUFR_Converter_config}
   echo "done with main_read : $date"

   coloc_nwp_config=${CONF_DIR}/colocNWP_gdas_${date}_${sensor}.config
   ${EXE2} < ${coloc_nwp_config}
   echo "done with colocNWP : $date"

   fwd_config=${CONF_DIR}/fwd_gdas_${date}_${sensor}.config
   ${EXE3} < ${fwd_config}
   echo "done with fwd : $date"

done

