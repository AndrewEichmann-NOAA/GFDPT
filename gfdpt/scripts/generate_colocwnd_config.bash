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

SFCFLG=0
if [ ${SENSORID} -lt 0 ]; then
 echo "surface flag"
 SFCFLG=1
fi

#--------------------------------------
# generate files list for GFS forecast and ECMWF analysis files
#--------------------------------------
./generate_NWP_list.sh $yyyymmdd

#--------------------------------------
# generate files list for FMSDR
#--------------------------------------

obs_wnd_list=${LIST_PATH}"/obs_wnd_${yyyymmdd}_${SATSENSOR}.list"

./generate_OBSWND_list.bash $date $obs_wnd_list

#-----------------------------------------
# namelist for running colocNWPwRad w/ GFS
#-----------------------------------------
coloc_nwp_config=${CONF_DIR}/colocNWPwnd_gfs_${yyyymmdd}_${SATSENSOR}.config
gfs_atm_list=${LIST_PATH}"/gfs_atm_${yyyymmdd}.list"
gfs_sfc_list=${LIST_PATH}"/gfs_sfc_${yyyymmdd}.list"
gfs_wnd_list=${LIST_PATH}"/gfs_wnd_${yyyymmdd}.list"

if [ ! -d "${EDR_DATA_PATH}/$date/" ]; then 
  echo "...Creating directory for output : " ${EDR_DATA_PATH}/$date/
  mkdir -p ${EDR_DATA_PATH}/$date/
fi 

sed -e "s/SURFACEFLG/${SFCFLG}/g" -e "s|LOG_PATH|${LOG_PATH}|" -e "s|OBS_WIND_LIST|$obs_wnd_list|" -e "s/DATE/$date/" -e "s|EDR_PATH|${EDR_DATA_PATH}|" ${CONF_TEMPL_DIR}/COLOC_NWPWND_Template.CONFIG  > tmpFile
sed -e "s|NWP_ATM_LIST|$gfs_atm_list|" -e "s/SENSORID/${SENSORID}/g" -e "s/NWPID/3/" -e "s/SATSENSOR/$SATSENSOR/" tmpFile  > tmpFile2
sed -e "s|NWP_SFC_LIST|$gfs_sfc_list|" -e "s|NWP_WIND_LIST|$gfs_wnd_list|" tmpFile2 > ${coloc_nwp_config}
rm  tmpFile  tmpFile2

#-------------------------------------------
# namelist for running colocNWPwRad w/ ECMWF
#-------------------------------------------
coloc_nwp_config=${CONF_DIR}/colocNWPwnd_ecmwf_${yyyymmdd}_${SATSENSOR}.config
ecmwf_atm_list=${LIST_PATH}"/ecmwf_atm_${yyyymmdd}.list"
ecmwf_sfc_list=${LIST_PATH}"/ecmwf_sfc_${yyyymmdd}.list"
ecmwf_wnd_list=${LIST_PATH}"/ecmwf_wnd_${yyyymmdd}.list"
sed -e "s/SURFACEFLG/${SFCFLG}/g" -e "s|LOG_PATH|${LOG_PATH}|" -e "s|OBS_WIND_LIST|$obs_wnd_list|" -e "s/DATE/$date/" -e "s|EDR_PATH|${EDR_DATA_PATH}|" ${CONF_TEMPL_DIR}/COLOC_NWPWND_Template.CONFIG  > tmpFile
sed -e "s|NWP_ATM_LIST|$ecmwf_atm_list|" -e "s/SENSORID/${SENSORID}/g" -e "s/NWPID/2/" -e "s/SATSENSOR/$SATSENSOR/" tmpFile  > tmpFile2
sed -e "s|NWP_SFC_LIST|$ecmwf_sfc_list|" -e "s|NWP_WIND_LIST|$ecmwf_wnd_list|" tmpFile2 > ${coloc_nwp_config}
rm  tmpFile  tmpFile2

exit 0