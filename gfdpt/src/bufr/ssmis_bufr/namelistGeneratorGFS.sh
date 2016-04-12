#!/bin/bash 
#########################################################
# Description:
#    Conveniently generate namelist files that are needed   
#    to ssmisBUFR_Converter.exe, which is convert SSMIS BUFR  
#    to FMSDR.
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Date: 5/12/2014
# Usage: ./generate_GFS_list.sh  20130607
#
#########################################################
date=$1
yyyy=${date:0:4}   # 0-based index, get year
mm=${date:4:2}     # get month
dd=${date:6}
tmp=`expr $dd - 1`  # prev day
dd2=`printf "%02i" $tmp`   # display as 2-digit with 0-pad in front

CONF_DIR='./config'
CONF_TEMPL_DIR='./config/template'

# TODO 
# figure out m2 and d2 in cases when crossing month.
# In most cases, m2 is the same as m1.

# Generate GFS forecast file list for ATM
sed -e "s/YYYY/$yyyy/" ${CONF_TEMPL_DIR}/GFS_ATM_LIST_Template.CONFIG  > tmpFile
sed -e "s/M1/$mm/"  tmpFile  > tmpFile2
sed -e "s/D1/$dd/"  tmpFile2 > tmpFile3
sed -e "s/M2/$mm/"  tmpFile3 > tmpFile4
sed -e "s/D2/$dd2/" tmpFile4 > ${CONF_DIR}/gfs_atm_list_${date}.config
rm tmpFile tmpFile2 tmpFile3  tmpFile4

# Generate GFS forecast file list for SFC
sed -e "s/YYYY/$yyyy/" ${CONF_TEMPL_DIR}/GFS_SFC_LIST_Template.CONFIG  > tmpFile
sed -e "s/M1/$mm/"  tmpFile  > tmpFile2
sed -e "s/D1/$dd/"  tmpFile2 > tmpFile3
sed -e "s/M2/$mm/"  tmpFile3 > tmpFile4
sed -e "s/D2/$dd2/" tmpFile4 > ${CONF_DIR}/gfs_sfc_list_${date}.config
rm tmpFile tmpFile2 tmpFile3  tmpFile4
