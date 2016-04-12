#!/bin/bash
###########################################################################
#
# (c) RTi 
#
# Purpose: 	Setup config files (namelists) for various observation 
#               assessment tool binaries
#
# Usg: 		generate_config.bash yyyymmdd
# Usg: 		generate_config.bash 20130722
#
# Author:	Eric Maddy RTi at JCSDA (Eric.Maddy@NOAA.gov) 2014-06-10
#
# Input: 	date in yyyymmdd format
#               must have user defined paths and instrument setup 
#               defined in instrument.setup and paths.setup
#
# Output:	namelists for forward calculations and NWP colocation in 
#               $CONFIG_PATH 
#               and list files for observations and NWP in $LIST_PATH
#
###########################################################################

#---export user setup
. instrument.setup
. paths.setup

#---check arguments (tbd)
yyyymmdd=$1
yyyy=`echo $yyyymmdd | cut -c1-4`
mm=`echo $yyyymmdd | cut -c5-6`
dd=`echo $yyyymmdd | cut -c7-8`
date=${yyyy}-${mm}-${dd}

CONF_DIR=${CONFIG_PATH}
CONF_TEMPL_DIR=${CONF_DIR}/template/

#-------------------------------------------
# Create namelist needed to run fwd with GFS
#-------------------------------------------

if [ ! -d "${FMSDR_SIM_DATA_PATH}/$date/" ]; then 
    echo "...Creating directory for output : " ${FMSDR_SIM_DATA_PATH}/$date/
    mkdir -p ${FMSDR_SIM_DATA_PATH}/$date/
fi 

edr_coloc_nwp_file=${LIST_PATH}/NWP_GFS_${yyyymmdd}_${SATSENSOR}.list 
ls -d -1 ${EDR_DATA_PATH}/${date}/NWP_GFS* > ${edr_coloc_nwp_file}
fwd_config=${CONF_DIR}/fwd_gfs_${yyyymmdd}_${SATSENSOR}.config
fmsdr_sim_rad_file=${LIST_PATH}/FWD_NWP_GFS_${yyyymmdd}_${SATSENSOR}.list 
sed -e "s|${EDR_DATA_PATH}/${date}|${FMSDR_SIM_DATA_PATH}/$date/|g" -e "s/NWP_GFS/FWD_NWP_GFS/g" ${edr_coloc_nwp_file} > ${fmsdr_sim_rad_file}
noise_file="noise_gdas_${yyyymmdd}_FMSDR_${SATSENSOR}.dat"
log_file="gdas_gfs_${yyyymmdd}_FMSDR_${SATSENSOR}.log"
 sed -e "s|EDR_COLOC_NWP_FILE|${edr_coloc_nwp_file}|"  ${CONF_TEMPL_DIR}/FWD_Template.CONFIG  > tmpFile
sed -e "s|COAT_PATH|${COAT_PATH}|" -e "s|FMSDR_SIM_RAD_FILE|${fmsdr_sim_rad_file}|"  tmpFile  >   tmpFile2
sed -e "s/NOISE_FILE/${noise_file}/"  tmpFile2  >   tmpFile3
sed -e "s|STATIC_DATA|${STATIC_DATA}|" -e "s|LOG_PATH|${LOG_PATH}|" -e "s/LOG_FILE/${log_file}/" -e "s/NCHAN/$NCHAN/g" -e "s/SENSORID/$SENSORID/" -e "s/SATSENSOR/$SATSENSOR/g"  tmpFile3  >   ${fwd_config}
rm tmpFile tmpFile2 tmpFile3

#---------------------------------------------
# Create namelist needed to run fwd with GDAS
#---------------------------------------------

edr_coloc_nwp_file=${LIST_PATH}/NWP_GDAS_${yyyymmdd}_${SATSENSOR}.list 
ls -d -1 ${EDR_DATA_PATH}/${date}/NWP_GDAS* > ${edr_coloc_nwp_file}
fwd_config=${CONF_DIR}/fwd_gdas_${yyyymmdd}_${SATSENSOR}.config
fmsdr_sim_rad_file=${LIST_PATH}/FWD_NWP_GDAS_${yyyymmdd}_${SATSENSOR}.list 
sed -e "s|${EDR_DATA_PATH}/${date}|${FMSDR_SIM_DATA_PATH}/$date/|g" -e "s/NWP_GDAS/FWD_NWP_GDAS/g" ${edr_coloc_nwp_file} > ${fmsdr_sim_rad_file}
noise_file="noise_gdas_${yyyymmdd}_FMSDR_${SATSENSOR}.dat"
log_file="gdas_gdas_${yyyymmdd}_FMSDR_${SATSENSOR}.log"
sed -e "s|EDR_COLOC_NWP_FILE|${edr_coloc_nwp_file}|"  ${CONF_TEMPL_DIR}/FWD_Template.CONFIG  > tmpFile
sed -e "s|COAT_PATH|${COAT_PATH}|" -e "s|FMSDR_SIM_RAD_FILE|${fmsdr_sim_rad_file}|"  tmpFile  >   tmpFile2
sed -e "s/NOISE_FILE/${noise_file}/"  tmpFile2  >   tmpFile3
sed -e "s|STATIC_DATA|${STATIC_DATA}|" -e "s|LOG_PATH|${LOG_PATH}|" -e "s/LOG_FILE/${log_file}/" -e "s/NCHAN/$NCHAN/g" -e "s/SENSORID/$SENSORID/" -e "s/SATSENSOR/$SATSENSOR/g"  tmpFile3  >   ${fwd_config}
rm tmpFile tmpFile2 tmpFile3
  
#---------------------------------------------
# Create namelist needed to run fwd with ECMWF
#---------------------------------------------

edr_coloc_nwp_file=${LIST_PATH}/NWP_ECMWF_${yyyymmdd}_${SATSENSOR}.list 
ls -d -1 ${EDR_DATA_PATH}/${date}/NWP_ECMW* > ${edr_coloc_nwp_file}
fwd_config=${CONF_DIR}/fwd_ecmwf_${yyyymmdd}_${SATSENSOR}.config
fmsdr_sim_rad_file=${LIST_PATH}/FWD_NWP_ECMWF_${yyyymmdd}_${SATSENSOR}.list 
sed -e "s|${EDR_DATA_PATH}/${date}|${FMSDR_SIM_DATA_PATH}/$date/|g" -e "s/NWP_ECMW/FWD_NWP_ECMW/g" ${edr_coloc_nwp_file} > ${fmsdr_sim_rad_file}
noise_file="noise_gdas_${yyyymmdd}_FMSDR_${SATSENSOR}.dat"
log_file="gdas_ecmwf_${yyyymmdd}_FMSDR_${SATSENSOR}.log"
sed -e "s|EDR_COLOC_NWP_FILE|${edr_coloc_nwp_file}|"  ${CONF_TEMPL_DIR}/FWD_Template.CONFIG  > tmpFile
sed -e "s|COAT_PATH|${COAT_PATH}|" -e "s|FMSDR_SIM_RAD_FILE|${fmsdr_sim_rad_file}|"  tmpFile  >   tmpFile2
sed -e "s/NOISE_FILE/${noise_file}/"  tmpFile2  >   tmpFile3
sed -e "s|STATIC_DATA|${STATIC_DATA}|" -e "s|LOG_PATH|${LOG_PATH}|" -e "s/LOG_FILE/${log_file}/" -e "s/NCHAN/$NCHAN/g" -e "s/SENSORID/$SENSORID/" -e "s/SATSENSOR/$SATSENSOR/g"  tmpFile3  >   ${fwd_config}
rm tmpFile tmpFile2 tmpFile3

#for edr_coloc_nwp_file in `ls -d -1 ${EDR_DATA_PATH}/${date}/NWP_ECMWdas_*`; do 
#  fwd_config=${CONF_DIR}/fwd_ecmwf`basename ${edr_coloc_nwp_file}`.config
#  fmsdr_sim_rad_file="${FMSDR_SIM_DATA_PATH}/${date}/sim_rad_`basename ${edr_coloc_nwp_file}`.dat"
#  noise_file="noise_gdas_${yyyymmdd}_FMSDR_${SATSENSOR}.dat"
#  log_file="gdas_ecmwf_${yyyymmdd}_FMSDR_${SATSENSOR}.log"
#  sed -e "s|EDR_COLOC_NWP_FILE|${edr_coloc_nwp_file}|"  ${CONF_TEMPL_DIR}/FWD_Template.CONFIG  > tmpFile
#  sed -e "s|FMSDR_SIM_RAD_FILE|${fmsdr_sim_rad_file}|"  tmpFile  >   tmpFile2
#  sed -e "s/NOISE_FILE/${noise_file}/"  tmpFile2  >   tmpFile3
#  sed -e "s|STATIC_DATA|${STATIC_DATA}|" -e "s|LOG_PATH|${LOG_PATH}|" -e "s/LOG_FILE/${log_file}/" -e "s/NCHAN/$NCHAN/g" -e "s/SENSORID/$SENSORID/" -e "s/SATSENSOR#/$SATSENSOR/g"  tmpFile3  >   ${fwd_config}
#  rm tmpFile tmpFile2 tmpFile3
#done 

exit 0