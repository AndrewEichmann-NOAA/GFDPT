#!/bin/bash 
#########################################################
#
# Description:
#    Colocate GFS or ECMWF forecast data to obs FMSDR
#    Forward model on colocated GFS or ECMWF to create simulated FMSDR
#
# Example: 
#         run_colocfwd.bash gcomw1_amsr2 20130722 
#
# Author: Eric Maddy (RTI) @ JCSDA,
#         Eric.Maddy@noaa.gov
# Date: 6/11/2014
#
#########################################################
sensor=$1
date=$2

#---export user setup
. instrument.setup
. paths.setup

CONF_DIR='config'
EXE1='../bin/colocNWPwRad'
EXE2='../bin/fwdlist'

#---generate namelists and list files for colocation codes
generate_coloc_config.bash ${date}
#exit
#---run collocation of ECMWF analysis to observation location
coloc_nwp_config=${CONF_DIR}/colocNWP_ecmwf_${date}_${sensor}.config
${EXE1} < ${coloc_nwp_config} 
echo "Finished with ECMWF colocNWP : $date"

#---run collocation of GFS 6 hr forecast to observation location
coloc_nwp_config=${CONF_DIR}/colocNWP_gfs_${date}_${sensor}.config
${EXE1} < ${coloc_nwp_config} 
echo "Finished with GFS colocNWP : $date"

#---run collocation of GDAS analysis to observation location
coloc_nwp_config=${CONF_DIR}/colocNWP_gdas_${date}_${sensor}.config
#${EXE1} < ${coloc_nwp_config}
echo "Finished with GFS colocNWP : $date"

#---wait for collocations to finish
wait 
#exit 0 

#---generate namelists and list files for fwdlist code
generate_fwd_config.bash ${date}

#---run fwd calculation with ECMWF analysis as input
fwd_config=${CONF_DIR}/fwd_ecmwf_${date}_${sensor}.config
${EXE2} < ${fwd_config} &
echo "Finished with ECMWF fwd : $date"

#---run fwd calculation with GFS 6hr forecast as input
fwd_config=${CONF_DIR}/fwd_gfs_${date}_${sensor}.config
echo "${EXE2} < ${fwd_config} &"
${EXE2} < ${fwd_config} &
echo "Finished with GFS fwd : $date"

#---run fwd calculation with GDAS analysis as input
fwd_config=${CONF_DIR}/fwd_gdas_${date}_${sensor}.config
#${EXE2} < ${fwd_config}
echo "Finished with GFS fwd : $date"

#---wait for forward calculations to finish
wait 

echo " ------- Files created --------"
echo "Modify the following in ../config/config_SensorParam_SAT_SENSOR.pro "
echo ""
echo "radlistfile1=${LIST_PATH}/obs_rad_${date}_${SATSENSOR}.list"
echo "scenelistfile=${LIST_PATH}/NWP_ECMWF_${date}_${SATSENSOR}.list"
echo "radlistfile2=${LIST_PATH}/FWD_NWP_ECMWF_${date}_${SATSENSOR}.list"

echo "radlistfile1=${LIST_PATH}/obs_rad_${date}_${SATSENSOR}.list"
echo "scenelistfile=${LIST_PATH}/NWP_GFS_${date}_${SATSENSOR}.list"
echo "radlistfile2=${LIST_PATH}/FWD_NWP_GFS_${date}_${SATSENSOR}.list"

