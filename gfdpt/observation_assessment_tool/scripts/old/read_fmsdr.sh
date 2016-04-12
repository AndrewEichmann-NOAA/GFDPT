#!/bin/bash 
#########################################################
# Description:
#    Read FMSDR files (obs / sim )
#
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Date: 5/12/2014
# Usage: ./read_fmsdr.sh  2013060700  obs gdas
#        ./read_fmsdr.sh  2013060700  obs gfs
#        ./read_fmsdr.sh  2013060700  sim gdas
#        ./read_fmsdr.sh  2013060700  sim gfs
#
#########################################################
# default to f18
date=$1 
obsFlag=$2 
gdasFlag=$3

CONF_DIR='config'
CONF_TEMPL_DIR='config/template'
FMSDR_OBS_RAD_DIR='fmsdr_obs_rad'
FMSDR_OBS_SIM_DIR='fmsdr_sim_rad'

if [ "$obsFlag" == "obs" ]
then 
   if [ "$gdasFlag" == "gdas" ]
   then 
      fmsdr_data="${FMSDR_OBS_RAD_DIR}\/ssmisu_gdas_${date}_FMSDR_f18.dat"
   else 
      fmsdr_data="${FMSDR_OBS_RAD_DIR}\/ssmisu_GFS_${date}_FMSDR_f18.dat"
   fi
else
   if [ "$gdasFlag" == "gdas" ]
   then 
      fmsdr_data="${FMSDR_OBS_SIM_DIR}\/sim_rad_gdas_${date}_FMSDR_f18.dat"
   else 
s
      fmsdr_data="${FMSDR_OBS_SIM_DIR}\/sim_rad_GFS_${date}_FMSDR_f18.dat"
   fi
fi


echo '-----'
echo ${fmsdr_data}
echo '-----'

# Generate namelist file
sed -e "s/FMSDR_FILE/${fmsdr_data}/g" ${CONF_TEMPL_DIR}/READ_FMSDR_Template.CONFIG > read_fmsdr.config

# Read FMSDR file to get basic information(header) with options to print
# out each record.
./read_fmsdr.exe < read_fmsdr.config
