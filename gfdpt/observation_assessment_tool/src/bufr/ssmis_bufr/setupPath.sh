#!/bin/bash 
#########################################################
# Description:
#    Make links to locaitons where data are stored. 
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Date: 5/30/2014
# Usage: ./setupPath.sh
#
#########################################################
#########################################################
##    Update these data location accordingly 
#########################################################
# 1. Observed radiance data 
OBS_RAD_DIR=/disk1/pub/dxu/fmsdr_obs_rad

# 2. Simulated radiance data
SIM_RAD_DIR=/disk1/pub/dxu/fmsdr_sim_rad

# 3. Colocated NWP GFS data 
EDR_COLOC_NWP_DIR=/disk1/pub/dxu/edr_coloc_nwp

ln -sf  ${OBS_RAD_DIR}       fmsdr_obs_rad
ln -sf  ${SIM_RAD_DIR}       fmsdr_sim_rad
ln -sf  ${EDR_COLOC_NWP_DIR} edr_coloc_nwp

