#!/bin/bash 
#########################################################
#
# Description:
#    Colocate GFS or ECMWF forecast data to obs FMSDR
#    Forward model on colocated GFS or ECMWF to create simulated FMSDR
#
# Example: 
#         run_colocwnd.bash gcomw1_amsr2 20130722 
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
EXE1='../bin/colocNWPsatwind'

#---generate namelists and list files for colocation codes
generate_colocwnd_config.bash ${date}

#---run collocation of ECMWF analysis to observation location
coloc_nwp_config=${CONF_DIR}/colocNWPwnd_ecmwf_${date}_${sensor}.config
${EXE1} < ${coloc_nwp_config}
echo "Finished with ECMWF colocNWP : $yyyymmdd"

#---run collocation of GFS 6 hr forecast to observation location
coloc_nwp_config=${CONF_DIR}/colocNWPwnd_gfs_${date}_${sensor}.config
#${EXE1} < ${coloc_nwp_config}
echo "Finished with GFS colocNWP : $yyyymmdd"

yyyy=`echo $date | cut -c1-4`
mm=`echo $date | cut -c5-6`
dd=`echo $date | cut -c7-8`
yyyymmdd=${yyyy}-${mm}-${dd}

#---create GFS list files for collocations 
NWP_LIST=${LIST_PATH}"/NWP_GFS_${date}_${SATSENSOR}.list"
NWP_WND_LIST=${LIST_PATH}"/NWPWND_GFS_${date}_${SATSENSOR}.list"
#echo "ls -d -1 ${EDR_DATA_PATH}/${SATSENSOR}/${yyyymmdd}/NWP_GFSWN*.bin > ${NWP_WND_LIST}"
#echo "sed 's/NWP_GFSWN/NWP_GFS/g' ${NWP_WND_LIST} > ${NWP_LIST}"

ls -d -1 ${EDR_DATA_PATH}/${yyyymmdd}/NWP_GFSWN*.bin > ${NWP_WND_LIST}
sed 's/NWP_GFSWN/NWP_GFS/g' ${NWP_WND_LIST} > ${NWP_LIST}

#---create ECMWF list files for collocations
NWP_LISTE=${LIST_PATH}"/NWP_ECM_${date}_${SATSENSOR}.list"
NWP_WND_LISTE=${LIST_PATH}"/NWPWND_ECM_${date}_${SATSENSOR}.list"

sed 's/NWP_GFSWN/NWP_ECMWN/g' ${NWP_WND_LIST} > ${NWP_WND_LISTE}
sed 's/NWP_GFS/NWP_ECM/g' ${NWP_LIST} > ${NWP_LISTE}
