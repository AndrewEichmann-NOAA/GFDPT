#!/bin/bash 
#########################################################
# Description:
#    Conveniently generate namelist files that are needed   
#    to ssmisBUFR_Converter.exe, which is convert SSMIS BUFR  
#    to FMSDR.
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Date: 5/12/2014
# Usage: ./generateConfig.sh   f16
#        ./generateConfig.sh   f17
#        ./generateConfig.sh   f18
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

sensor=$1

# Specify the dates you want to run
dates="2013060700 2013060706 "

# default to f18
satid="286" 
if [ "$sensor" == "f16" ]
then 
   satid="249" 
fi 

if [ "$sensor" == "f17" ]
then 
   satid="285" 
fi 

if [ "$sensor" == "f18" ]
then 
   satid="286" 
fi 

CONF_DIR='config'
CONF_TEMPL_DIR='config/template'
FMSDR_OBS_RAD_DIR='fmsdr_obs_rad'
FMSDR_SIM_RAD_DIR='fmsdr_sim_rad'
EDR_COLOC_NWP_DIR='edr_coloc_nwp'

# GDAS BUFR : 4 / day at 00Z, 06Z, 12Z and 18Z
#-----------------------------------------
# Give dates (user-specified) 
#-----------------------------------------
for date in $dates
do 
   yyyymmdd=${date:0:8}   # "20130607
   # 1. Create namelist to main_read_ssmis.exe
   main_read_ssmis_config=${CONF_DIR}/ssmisu_gdas_${date}_${sensor}.config
   sed -e "s/YYYYMMDDHH/$date/" ${CONF_TEMPL_DIR}/SSMISU_Template.CONFIG  > tmpFile
   sed -e "s/XXX/$satid/"  tmpFile   >  tmpFile2
   sed -e "s/YYY/$sensor/" tmpFile2  >  ${main_read_ssmis_config}

   # 2. Create namelist needed to run colocNWPwRad
   #--------------
   # obs rad file
   #--------------
   fmsdr_obs_rad_file=${FMSDR_OBS_RAD_DIR}/ssmisu_gdas_${date}_FMSDR_${sensor}.dat
   fmsdr_obs_list=${CONF_DIR}/obs_rad_gdas_${date}_${sensor}.config
   echo "${fmsdr_obs_rad_file}" > ${fmsdr_obs_list}
   rm tmpFile tmpFile2 

   #---------------------
   # GFS forecast files
   #---------------------
   ./namelistGeneratorGFS.sh   $yyyymmdd

   #-------------------------------------
   # namelist for running colocNWPwRad
   #-------------------------------------
   coloc_nwp_config=${CONF_DIR}/colocNWP_gdas_${date}_${sensor}.config
   obs_rad_list="obs_rad_gdas_${date}_${sensor}.config"
   gfs_atm_list="gfs_atm_list_${yyyymmdd}.config"
   gfs_sfc_list="gfs_sfc_list_${yyyymmdd}.config"
   sed -e "s/OBS_RAD_FILE/$obs_rad_list/" ${CONF_TEMPL_DIR}/COLOC_NWP_Template.CONFIG  > tmpFile
   sed -e "s/GFS_ATM_LIST/$gfs_atm_list/" tmpFile  > tmpFile2
   sed -e "s/GFS_SFC_LIST/$gfs_sfc_list/" tmpFile2 > ${coloc_nwp_config}
   rm  tmpFile  tmpFile2

   # 3. Create namelist needed to run fwd
   fwd_config=${CONF_DIR}/fwd_gdas_${date}_${sensor}.config
   edr_coloc_nwp_file="${EDR_COLOC_NWP_DIR}\/NWP_GFSgdas_${date}_FMSDR_${sensor}.dat"
   fmsdr_sim_rad_file="${FMSDR_SIM_RAD_DIR}\/sim_rad_gdas_${date}_FMSDR_${sensor}.dat"
   noise_file="nosie_gdas_${date}_FMSDR_${sensor}.dat"
   log_file="gdas_${date}_FMSDR_${sensor}.log"
   sed -e "s/EDR_COLOC_NWP_FILE/${edr_coloc_nwp_file}/"  ${CONF_TEMPL_DIR}/FWD_Template.CONFIG  > tmpFile
   sed -e "s/FMSDR_SIM_RAD_FILE/${fmsdr_sim_rad_file}/"  tmpFile  >   tmpFile2
   sed -e "s/NOISE_FILE/${noise_file}/"  tmpFile2  >   tmpFile3
   sed -e "s/LOG_FILE/${log_file}/"    tmpFile3  >   ${fwd_config}
   rm tmpFile tmpFile2 tmpFile3
    
done

