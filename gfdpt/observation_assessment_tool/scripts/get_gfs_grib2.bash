#!/bin/bash

####################################################################################################
#
# Purpose: 	To extract GFS(used to called AVN) data using wgrib into binary format.
#               It requires wgrib2 and IDL.
#
#
# Usg: 		get_gfs_grib2.bash yyyymmdd   or   get_gfs_grib2.bash
# Usg: 		get_gfs_grib2.bash 20111108   or   get_gfs_grib2.bash
#
# Author:	Eric.Maddy RTi at JCSDA (Eric.Maddy@NOAA.gov) 2014-05-08
#               added 10-70mb output from grib.               2014-07-24
#
# Input: 	GFS model output files in grib format using 6 hour forecast files
#               example: gblav.PGrbF06.140423.00z  
#                        gblav.PGrbF06.140415.06z  
#                        gblav.PGrbF06.140408.12z  
#                        gblav.PGrbF06.140430.18z
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

cmd2=/net/orbit272l/home/pub/external_data/ecmwf/wgrib2
IDL=/usr/local/bin/idl
dir_in=/net/orbit001x/disk011/pub/ancillary/avn/
dir_in=/data/data086/pub/ancillary/gfs
dir_out=${NWP_DATA_PATH}/gfs


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



hhs=(00 06 12 18)
hh1s=(18 00 06 12)  # no PCP in F00 files, so we use F06 files, but shifted by -6 hours

for (( ih=0; ih<4; ih++ )) ; do
  
  hh=${hhs[$ih]}
  hh1=${hh1s[$ih]}
  echo "$hh $hh1"
  dir_in1=${dir_in}/${yyyy}-${mm2}-${dd2}
  dir_in2=${dir_in}/${yyyy1}-${mm2}-${dd21} #${yyyy1}/${mm21}
  
  #  file_grib=${dir_in1}/gblav.PGrbF06.${yy2}${mm2}${dd2}.${hh}z
  file_grib=${dir_in1}/gfs.t${hh}z.pgrbf06.grib2
  
  if [[ ${ih} -eq 0 ]] ; then
    file_rain=${dir_in2}/gfs.t${hh}z.pgrbf06.grib2  #gblav.PGrbF06.${yy21}${mm21}${dd21}.${hh1}z   #T${hh1}Z.GFS_${yyyy1}_${jjj1}
  else
    file_rain=${dir_in1}/gfs.t${hh}z.pgrbf06.grib2  #gblav.PGrbF06.${yy2}${mm2}${dd2}.${hh1}z      #T${hh1}Z.GFS_${yyyy}_${jjj}
  fi
  
  if [[ -s ${file_grib} && -s ${file_rain} ]] ; then
  
  file_atm=gfs_atm${ymd}.t${hh}
  file_atm=gfs_atm${yyyy}-${mm2}-${dd2}.t${hh}
  file_sfc=gfs_sfc${ymd}.t${hh}
  file_sfc=gfs_sfc${yyyy}-${mm2}-${dd2}.t${hh}
 
#  echo "${cmd2}  ${file_grib} | grep ':TMP:kpds' | grep '100 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns   -bin  ${dir_out}/${file_atm}.le"

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns   -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep ':20 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep ':30 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep ':50 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep ':70 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '100 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '150 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '200 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '250 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '300 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '350 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null |  grep ':TMP:' | grep '400 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '450 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '500 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '550 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '600 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '650 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '700 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '750 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '800 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '850 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '900 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '925 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '950 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '975 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':TMP:' | grep '1000 mb:' |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:' | grep ':10 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:' | grep ':30 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:' | grep ':50 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:' | grep ':70 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '100 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '150 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '200 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '250 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '300 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '350 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '400 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '450 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '500 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '550 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '600 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '650 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '700 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '750 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '800 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '850 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null| grep ':RH:'  | grep '900 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '925 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '950 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '975 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':RH:'  | grep '1000 mb:' |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_atm}.le


  ${cmd2} -s  ${file_grib} 2> /dev/null | grep ':LAND:surface:'       		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns          -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':PRES:surface:'       		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':TMP:surface:'        		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':TMP:0-0.1 m below ground:' 		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':PWAT:entire atmosphere ' 		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':CWAT:entire atmosphere' 		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':WEASD:surface:'      		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':ICEC:surface:'       		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':UGRD:10 m above ground:'	|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':VGRD:10 m above ground:'	|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_grib} 2> /dev/null | grep ':RH:2 m above ground:'		|  ${cmd2} -i ${file_grib}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_rain} 2> /dev/null | grep ':PRATE:surface:'       		|  ${cmd2} -i ${file_rain}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  ${cmd2} -s ${file_rain} 2> /dev/null | grep ':APCP:surface:'       		|  ${cmd2} -i ${file_rain}  -no_header -order we:ns  -append -bin  ${dir_out}/${file_sfc}.le
  

${IDL} <<EOL
.r convert_endian.pro
convert_endian, "${dir_out}/${file_atm}.le", "${dir_out}/${file_atm}", 52
convert_endian, "${dir_out}/${file_sfc}.le", "${dir_out}/${file_sfc}", 13
exit
EOL

  rm -f ${dir_out}/${file_atm}.le ${dir_out}/${file_sfc}.le
  
  else
    
    echo "no input file:"
    echo ${file_grib} 
    echo ${file_rain}
  
  fi
  
done
