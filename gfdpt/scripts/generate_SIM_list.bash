#!/bin/bash

###########################################################################
#
# (c) RTi 
#
# Purpose: 	Setup list files for simulated observations
#
# Usg: 		generate_SIM_list.bash yyyymmdd
# Usg: 		generate_SIM_list.bash 20130722
#
# Author:	Eric Maddy RTi at JCSDA (Eric.Maddy@NOAA.gov) 2014-06-10
#
# Input: 	date in yyyymmdd format
#               must have user defined paths and instrument setup 
#               defined in instrument.setup and paths.setup
#
# Output:	list files for observations in $LIST_PATH
#
###########################################################################

date=$1
obs_rad_list=$2

#---export user setup
. instrument.setup
. paths.setup

FMSDR_SIM_PATH=${FMSDR_SIM_DATA_PATH}/${date}

ls -d -1 ${FMSDR_SIM_PATH}/*GFS* > ${obs_rad_list}

