#!/bin/bash

####################################################################################################
#
# Purpose: 	To extract GFS(used to called AVN) wind fields using wgrib into binary format.
#               It requires wgrib2 and IDL.  
#
#
# Usg: 		get_gfs_grib2wind.bash yyyymmdd   or   get_gfs_grib2wind.bash
# Usg: 		get_gfs_grib2wind.bash 20111108   or   get_gfs_grib2wind.bash
#
# Author:	Eric.Maddy RTi at JCSDA (Eric.Maddy@NOAA.gov) 2014-05-08
#
# Input: 	GFS model output files in grib format
#               example: gblav.PGrbF06.140423.00z  
#                        gblav.PGrbF06.140415.06z  
#                        gblav.PGrbF06.140408.12z  
#                        gblav.PGrbF06.140430.18z
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
dir_in=/net/orbit001x/disk011/pub/ancillary/avn
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
  yy21=`echo ${yyy1} | cut -c3-4`
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
  dir_in1=${dir_in}/${yyyy}/${mm2}
  dir_in2=${dir_in}/${yyyy1}/${mm21}
  file_grib=${dir_in1}/gfs.${hh}z.pgrbf06
  
  if [[ -s ${file_grib} ]] ; then
  
  file_wnd=gfs_wnd${yyyy}-${mm2}-${dd2}.t${hh}
  
  -no_header -order we:ns
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '100 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns     	 -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '150 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '200 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '250 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '300 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '350 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '400 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '450 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '500 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '550 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '600 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '650 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '700 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '750 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '800 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '850 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '900 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '925 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '950 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '975 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} | grep ':UGRD:kpds' | grep '1000 mb:' |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le

  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '100 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '150 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '200 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '250 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '300 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '350 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '400 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '450 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '500 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '550 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '600 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '650 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '700 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '750 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '800 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '850 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '900 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '925 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '950 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '975 mb:'  |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le
  ${cmd2}  ${file_grib} 2> /dev/null | grep ':VGRD:kpds'  | grep '1000 mb:' |  ${cmd2} -i ${file_grib} -no_header -order we:ns  -append -bin  ${dir_out}/${file_wnd}.le

${IDL} <<EOL
.r convert_endian.pro
convert_endian, "${dir_out}/${file_wnd}.le", "${dir_out}/${file_wnd}", 42
exit
EOL

  rm -f ${dir_out}/${file_wnd}.le 
  
  else
    
    echo "no input file:"
    echo ${file_grib} 
  
  fi
  
done
