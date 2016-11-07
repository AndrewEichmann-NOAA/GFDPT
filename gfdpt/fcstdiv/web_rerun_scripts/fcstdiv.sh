#!/bin/sh

## RUN FORECAST-FORECAST CORRELATION JOB FOR FORECAST DIVERGENCE WEBSITE.

sh /nco/pmb/f-f-divergence/scripts/job_corr_gfs_vs_ecmwf_0012z.sh > /ptmp/wx12ss/vsdb_v07_messages/logfile_vsdb0012 2>&1

## RUN EXTREME ANALYSIS DIFFERENCES JOB FOR FORECAST DIVERGENCE WEBSITE.

sh /nco/pmb/f-f-divergence/scripts/run_job_gfs_ecmwf_gribextremes_00.sh > /ptmp/wx12ss/vsdb_v07_messages/logfile_gribextremes00 2>&1

## COPY ALL FORECAST CORRELATION FILES TO LNX321 INTRANET SERVER

sh /u/wx12ss/kam/scripts/cp_ext_anl.sh > /ptmp/wx12ss/fcstdiv/logfiles/cp_ext_anl.out 2>&1

## COPY ALL EXTREME ANALYSIS DIFFERENCE FILES TO LNX321 INTRANET SERVER

sh /u/wx12ss/kam/scripts/cp_fcstcor.sh > /ptmp/wx12ss/fcstdiv/logfiles/cp_fcstcor.out

## RUN SCRIPT TO UPDATE FORECAST CORRELATION SECTION OF THE WEBSITE

sh /u/wx12ss/kam/scripts/fcstdiv_correlations_web_update.sh > /ptmp/wx12ss/fcstdiv/logfiles/cp_corr_update.out 2>&1

## RUN SCRIPT TO UPDATE EXTREME ANALYSIS SECTION OF THE WEBSITE

sh /u/wx12ss/kam/scripts/fcstdiv_ext_anl_web_update.sh > /ptmp/wx12ss/fcstdiv/logfiles/cp_extanl_update.out 2>&1

## RUN SCRIPT TO CLEAN UP THE ARCHIVE (31 DAYS ONLY)

sh /u/wx12ss/kam/scripts/archive_update_ccs.sh > /ptmp/wx12ss/fcstdiv/logfiles/archive_script 2>&1

## RUN SCRIPT TO COPY NEW VSDB AND GRIB FILES TO PROD CCS

sh /u/wx12ss/kam/scripts/scp_grib_vsdb.sh > /ptmp/wx12ss/fcstdiv/logfiles/vsdb_grib_scp.out 2>&1
