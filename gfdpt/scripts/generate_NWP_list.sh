#!/bin/bash 
###########################################################################
#
# (c) RTi 
#
# Purpose: 	Setup list files for NWP
#
# Usg: 		generate_NWP_list.bash yyyymmdd
# Usg: 		generate_NWP_list.bash 20130722
#
# Author:	Eric Maddy RTi at JCSDA (Eric.Maddy@NOAA.gov) 2014-06-10
#               based on generate_GFS_list.sh by Deyong Xu (RTI) @ JCSDA,
#               Deyong.Xu@noaa.gov
#
#
# Input: 	date in yyyymmdd format
#               must have user defined paths and instrument setup 
#               defined in instrument.setup and paths.setup
#
# Output:	list files for observations and NWP in $LIST_PATH
#
# History:      5/30/2014 -- grab previous day ESM
#               6/11/2014 -- extend for ECMWF and OAT 
#               6/12/2014 -- fix for dates that straddle years or months
#
########################################################################
. instrument.setup
. paths.setup

date=$1
yyyy=${date:0:4}   # 0-based index, get year
mm=${date:4:2}     # get month
dd=${date:6}

ym1=`date +%Y --date="${yyyy}-${mm}-${dd} -1 day"`
mm1=`date +%m --date="${yyyy}-${mm}-${dd} -1 day"`
dd1=`date +%d --date="${yyyy}-${mm}-${dd} -1 day"`

ym2=`date +%Y --date="${yyyy}-${mm}-${dd} +1 day"`
mm2=`date +%m --date="${yyyy}-${mm}-${dd} +1 day"`
dd2=`date +%d --date="${yyyy}-${mm}-${dd} +1 day"`

#echo ${yyyy}${mm}${dd}
#echo ${ym1}${mm1}${dd1}
#echo ${ym2}${mm2}${dd2}

CONF_DIR=${CONFIG_PATH}
CONF_TEMPL_DIR=${CONF_DIR}/template

# TODO 
# figure out m2 and d2 in cases when crossing month.
# In most cases, m2 is the same as m1.

# Generate GFS forecast file list for ATM
sed -e "s|NWP_PATH|${NWP_DATA_PATH}|g" -e "s/YYYY1/$yyyy/" -e "s/YYYY0/$ym1/" -e "s/YYYY2/$ym2/" ${CONF_TEMPL_DIR}/GFS_ATM_LIST_Template.CONFIG  > tmpFile
sed -e "s/M0/$mm1/" -e "s/D0/$dd1/"  tmpFile  > tmpFile2
sed -e "s/M1/$mm/" -e "s/D1/$dd/"  tmpFile2 > tmpFile3
sed -e "s/M2/$mm2/" -e "s/D2/$dd2/" tmpFile3 > ${LIST_PATH}/gfs_atm_${date}.list
#sed -e "s/D2/$dd2/" tmpFile5 > 
rm tmpFile tmpFile2 tmpFile3 # tmpFile4 tmpFile5

# Generate GFS forecast file list for SFC
sed -e "s|NWP_PATH|${NWP_DATA_PATH}|g" -e "s/YYYY1/$yyyy/" -e "s/YYYY0/$ym1/" -e "s/YYYY2/$ym2/" ${CONF_TEMPL_DIR}/GFS_SFC_LIST_Template.CONFIG  > tmpFile
sed -e "s/M0/$mm1/" -e "s/D0/$dd1/"  tmpFile  > tmpFile2
sed -e "s/M1/$mm/" -e "s/D1/$dd/"  tmpFile2 > tmpFile3
sed -e "s/M2/$mm2/" -e "s/D2/$dd2/" tmpFile3 > ${LIST_PATH}/gfs_sfc_${date}.list

#sed -e "s|NWP_PATH|${NWP_DATA_PATH}|g" -e "s/YYYY/$yyyy/" ${CONF_TEMPL_DIR}/GFS#_SFC_LIST_Template.CONFIG  > tmpFile
#sed -e "s/M1/$mm/"  tmpFile  > tmpFile2
#sed -e "s/D1/$dd/"  tmpFile2 > tmpFile3
#sed -e "s/M2/$mm/"  tmpFile3 > tmpFile4
#sed -e "s/D0/$dd0/" tmpFile4 > tmpFile5
#sed -e "s/D2/$dd2/" tmpFile5 > ${LIST_PATH}/gfs_sfc_${date}.list
rm tmpFile tmpFile2 tmpFile3 # tmpFile4 tmpFile5

# Generate GFS forecast file list for WND
sed -e "s|NWP_PATH|${NWP_DATA_PATH}|g" -e "s/YYYY1/$yyyy/" -e "s/YYYY0/$ym1/" -e "s/YYYY2/$ym2/" ${CONF_TEMPL_DIR}/GFS_WND_LIST_Template.CONFIG  > tmpFile
sed -e "s/M0/$mm1/" -e "s/D0/$dd1/"  tmpFile  > tmpFile2
sed -e "s/M1/$mm/" -e "s/D1/$dd/"  tmpFile2 > tmpFile3
sed -e "s/M2/$mm2/" -e "s/D2/$dd2/" tmpFile3 > ${LIST_PATH}/gfs_wnd_${date}.list

#sed -e "s|NWP_PATH|${NWP_DATA_PATH}|g" -e "s/YYYY/$yyyy/" ${CONF_TEMPL_DIR}/GFS#_WND_LIST_Template.CONFIG  > tmpFile
#sed -e "s/M1/$mm/"  tmpFile  > tmpFile2
#sed -e "s/D1/$dd/"  tmpFile2 > tmpFile3
#sed -e "s/M2/$mm/"  tmpFile3 > tmpFile4
#sed -e "s/D0/$dd0/" tmpFile4 > tmpFile5
#sed -e "s/D2/$dd2/" tmpFile5 > ${LIST_PATH}/gfs_wnd_${date}.list
#rm tmpFile tmpFile2 tmpFile3  tmpFile4 tmpFile5
rm tmpFile tmpFile2 tmpFile3 # tmpFile4 tmpFile5

# Generate lists for ECMWF files
sed 's/gfs/ecmwf/g' ${LIST_PATH}/gfs_sfc_${date}.list > ${LIST_PATH}/ecmwf_sfc_${date}.list
sed 's/gfs/ecmwf/g' ${LIST_PATH}/gfs_atm_${date}.list > ${LIST_PATH}/ecmwf_atm_${date}.list
sed 's/gfs/ecmwf/g' ${LIST_PATH}/gfs_wnd_${date}.list > ${LIST_PATH}/ecmwf_wnd_${date}.list
