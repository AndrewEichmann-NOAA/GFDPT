#!/bin/bash

####################################################################################################
#
# Purpose: 	To extract GFS(used to called AVN) data using wgrib into binary format.
#               It requires wgrib and IDL.
#
#
# Usg: 		get_gfs_grib1.bash yyyymmdd   or   get_gfs_grib1.bash
# Usg: 		get_gfs_grib1.bash 20111108   or   get_gfs_grib1.bash
#
# Author:	Eric.Maddy RTi at JCSDA (Eric.Maddy@NOAA.gov) 2014-05-08
#               added 10-70mb output from grib.               2014-07-24
#               for RH there is no 20mb info in the grib.  We repeat the 
#               10mb layer twice so that the reader for the binary files
#               is less complicated.
#               add O3 output from the grib.                  2014-10-09
#
# Input: 	GFS model output files in grib format using three hour forecast files
#               example: gblav.PGrbF03.140423.00z  
#                        gblav.PGrbF03.140415.06z  
#                        gblav.PGrbF03.140408.12z  
#                        gblav.PGrbF03.140430.18z
#    ;          0z    3z    6z    9z   12z   15z   18z   21z
#    ;   idx    0     1     2     3     4     5     6     7
#    anal1 = ['18z','00z','00z','06z','06z','12z','12z','18z']
#    fcst1 = ['F06','F03','F06','F03','F06','F03','F06','F03']
#    anal2 = ['18z','00z','00z','06z','06z','12z','12z','18z']
#    fcst2 = ['F09','F06','F09','F06','F09','F06','F09','F06']
#
# Output:	Binary surface data and leveled data, and we convert into big-endian format by 
#               calling IDL code convert_endian.pro
#		example:
#
# Some record in grib files:
#
####################################################################################################

#---export user setup
. instrument.setup
. paths.setup

#### User Definiton Section ####

cmd2=/net/orbit272l/home/pub/external_data/ecmwf/wgrib
cmd1=/net/orbit272l/home/pub/external_data/ecmwf/wgrib2
IDL=/usr/local/bin/idl
dir_in=/net/orbit001x/disk011/pub/ancillary/avn/
dir_in=/data/scdr035/ancillary/avn/
dir_out=${NWP_DATA_PATH}/gfs/


if [[ $# -ge 1 ]] ; then
  ymd=$1
  yyyy=`echo ${ymd} | cut -c1-4`
  jjj=`date -d "${ymd}" +%j`
  yyyy1=`date -d "${ymd} -1 day" +%Y`
  jjj1=`date -d "${ymd} -1 day" +%j`
  mm21=`date -d "${ymd} -1 day" +%m`
  dd21=`date -d "${ymd} -1 day" +%d`

  yy2=`echo ${ymd} | cut -c3-4`
  yy21=`echo ${yyyy1} | cut -c3-4`
  mm2=`echo ${ymd} | cut -c5-6`
  dd2=`echo ${ymd} | cut -c7-8`
else
  ymd=`date --date='1 day ago' +%Y-%m-%d`
  yyyy=`date --date='1 day ago' +%Y`
  jjj=`date --date='1 day ago' +%j`
  yyyy1=`date --date='2 days ago' +%Y`
  jjj1=`date --date='2 days ago' +%j`
  yy2=`echo ${ymd} | cut -c3-4`
  yy21=`echo ${yyy1} | cut -c3-4`
  mm2=`echo ${ymd} | cut -c5-6`
  dd2=`echo ${ymd} | cut -c7-8`
  mm21=`date -d "${ymd} -1 day" +%m`
  dd21=`date -d "${ymd} -1 day" +%d`
fi
echo "${ymd}  ${jjj} ${yy2} ${yy21}"


if [ $yyyy -ge 2015 ]; then
  vsoil='TSOIL'
else
  vsoil='TMP'
fi

hhs=(00 06 12 18)
hh1s=(18 00 06 12)  # no PCP in F00 files, so we use F06 files, but shifted by -6 hours

for (( ih=0; ih<4; ih++ )) ; do
  
  hh=${hhs[$ih]}
  hh1=${hh1s[$ih]}
  echo "$hh $hh1"
  dir_in1=${dir_in}/${yyyy}/${mm2}
  dir_in2=${dir_in}/${yyyy1}/${mm21}
  
  file_grib=${dir_in1}/gblav.PGrbF06.${yy2}${mm2}${dd2}.${hh}z
  
  if [[ ${ih} -eq 0 ]] ; then
    file_rain=${dir_in2}/gblav.PGrbF06.${yy21}${mm21}${dd21}.${hh1}z   #T${hh1}Z.GFS_${yyyy1}_${jjj1}
  else
    file_rain=${dir_in1}/gblav.PGrbF06.${yy2}${mm2}${dd2}.${hh1}z      #T${hh1}Z.GFS_${yyyy}_${jjj}
  fi
  
  if [[ -s ${file_grib} && -s ${file_rain} ]] ; then
  
  file_atm=gfs_atm${ymd}.t${hh}
  file_atm=gfs_atm${yyyy}-${mm2}-${dd2}.t${hh}
  file_sfc=gfs_sfc${ymd}.t${hh}
  file_sfc=gfs_sfc${yyyy}-${mm2}-${dd2}.t${hh}
  
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":LSM:"  |  ${cmd1} -4yr -i ${file_grib} -nh         -o ${file_sfc_le}
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":SP:"   |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":SKT:"  |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":2T:"   |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":10U:"  |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":10V:"  |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
# 
#  #---- atm ----
#  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":LSM:"  |  ${cmd1} -4yr -i ${file_grib} -nh -o ${file_atm_le}
#  
  echo "${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '10 mb:'  |  ${cmd2} -i ${file_grib} -nh -o  ${dir_out}/${file_atm}.le"
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -nh         -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep ':20 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep ':30 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep ':50 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep ':70 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep ':100 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '150 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '200 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '250 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '300 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '350 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '400 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '450 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '500 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '550 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '600 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '650 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '700 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '750 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '800 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '850 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '900 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '925 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '950 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '975 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:kpds' | grep '1000 mb:' |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  # note that there is no RH for 20 mb.  Repeat the 10 mb value so that the reader is less complicated.
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds' | grep ':30 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds' | grep ':50 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds' | grep ':70 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep ':100 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep ':150 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep ':200 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '250 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '300 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '350 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '400 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '450 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '500 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '550 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '600 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '650 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '700 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '750 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '800 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '850 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '900 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '925 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '950 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '975 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:kpds'  | grep '1000 mb:' |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
# ozone layers only down to 100 mb -- below this point?   
#  ${cmd2}  ${file_grib} 2> /dev/null | grep ':O3MR:kpds' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
#  ${cmd2}  ${file_grib} 2> /dev/null | grep ':O3MR:kpds' | grep ':20 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
#  ${cmd2}  ${file_grib} 2> /dev/null | grep ':O3MR:kpds' | grep ':30 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
#  ${cmd2}  ${file_grib} 2> /dev/null | grep ':O3MR:kpds' | grep ':50 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
#  ${cmd2}  ${file_grib} 2> /dev/null | grep ':O3MR:kpds' | grep ':70 mb:'  |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le
#  ${cmd2}  ${file_grib} 2> /dev/null | grep ':O3MR:kpds' | grep ':100 mb:' |  ${cmd2} -i ${file_grib} -nh -append -o  ${dir_out}/${file_atm}.le


#  echo "${cmd2} -s  ${file_grib} 2> /dev/null | grep ':LAND:sfc:'       		|  ${cmd2} -i ${file_grib}  -nh         -o  ${dir_out}/${file_sfc}.le"
#  exit
  ${cmd2} -s  ${file_grib} 2> /dev/null | grep ':LAND:sfc:'       		|  ${cmd2} -i ${file_grib}  -nh         -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':PRES:sfc:'       		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':TMP:sfc:'        		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ":${vsoil}:0-10 cm down:" 		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  
#  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':TSOIL:0-10 cm down:' 		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le

  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':PWAT:atmos col:' 		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':CWAT:atmos col:' 		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':WEASD:sfc:'      		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':ICEC:sfc:'       		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':UGRD:10 m above gnd:'	        |  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':VGRD:10 m above gnd:'	        |  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':RH:2 m above gnd:'		|  ${cmd2} -i ${file_grib}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_rain} 2> /dev/null | grep ':PRATE:sfc:'       		|  ${cmd2} -i ${file_rain}  -nh -append -o  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_rain} 2> /dev/null | grep ':APCP:sfc:'       		|  ${cmd2} -i ${file_rain}  -nh -append -o  ${dir_out}/${file_sfc}.le
  
sleep 2s
${IDL} <<EOL
.r convert_endian.pro
convert_endian, "${dir_out}/${file_atm}.le", "${dir_out}/${file_atm}", 52
convert_endian, "${dir_out}/${file_sfc}.le", "${dir_out}/${file_sfc}", 13
exit
EOL

 # rm -f ${dir_out}/${file_atm}.le ${dir_out}/${file_sfc}.le
  
  else
    
    echo "no input file:"
    echo ${file_grib} 
    echo ${file_rain}
  
  fi
  
done
