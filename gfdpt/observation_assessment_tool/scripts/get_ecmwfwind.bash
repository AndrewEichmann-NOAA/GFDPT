#! /bin/bash
########################################################################
# Purpose: 
#	Dumps wind forecast/analysis data for ECMWF.  Based on
#       get_ecmwf.bash by Wanchun Chen.
# 
# Updated: Extract both grib1 and grib2 data.
#          ECMWF data is updated to mixed grib1 and grib2 since
#          18 May 2011.   2011-05-18 00 hour file is still grib1 fomat,
#          2011-05-18 06 hour file changed to grib1 & grib2 mixed format.  
#
# Author:  
#	Eric S. Maddy, RTi at JCSDA
#
# Ref:
#  http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/convert_wgrib2.html   
#  
# Date:
#	04/02/2014
#
# Usg: get_ecmwfwind.bash yyyymmdd
# Exp: get_ecmwfwind.bash 20110530
#
########################################################################

#---export user setup
. instrument.setup
. paths.setup

# if has yyyymmdd command argument, the use it; otherwise, set 4 days ago.
if [[ $# -eq 1 ]] ; then
  yyyymmdd=$1
else
  yyyymmdd=`date --date='1 days ago' +%Y%m%d`
fi

yyyy=`echo $yyyymmdd | cut -c1-4`
mm=`echo $yyyymmdd | cut -c5-6`
dd=`echo $yyyymmdd | cut -c7-8`


UAD=/net/orbit001x/disk005/pub/ancillary/ecmwf/${yyyy}/${mm}
UAD=/data/scdr034/ancillary/ecmwf/${yyyy}/${mm}
cmd1=/net/orbit272l/home/pub/external_data/ecmwf/wgrib
cmd2=/net/orbit272l/home/pub/external_data/ecmwf/wgrib2


endian=0 # little endian
#endian=1 # big endian

IDL="/usr/local/bin/idl -quiet"

# check upstream to see if needed ecmwf files exist or not

file1=${UAD}/UAD${mm}${dd}0000${mm}${dd}00001.gz
file2=${UAD}/UAD${mm}${dd}0600${mm}${dd}06001.gz
file3=${UAD}/UAD${mm}${dd}1200${mm}${dd}12001.gz
file4=${UAD}/UAD${mm}${dd}1800${mm}${dd}18001.gz

if [[ ! -s ${file1} ]] ; then
  echo "Error: ${file1} NOT exist"
  exit 1
fi

if [[ ! -s ${file2} ]] ; then
  echo "Error: ${file2} NOT exist"
  exit 1
fi

if [[ ! -s ${file3} ]] ; then
  echo "Error: ${file3} NOT exist"
  exit 1
fi

if [[ ! -s ${file4} ]] ; then
  echo "Error: ${file4} NOT exist"
  exit 1
fi

declare -a odir 
odir=${NWP_DATA_PATH}/ecmwf/
cd ${odir}

if [[ ! -s ./UAD${mm}${dd}0000${mm}${dd}00001 ]]; then 
  cp ${UAD}/UAD${mm}${dd}0000${mm}${dd}00001.gz .
fi
if [[ ! -s ./UAD${mm}${dd}0600${mm}${dd}06001 ]]; then 
  cp ${UAD}/UAD${mm}${dd}0600${mm}${dd}06001.gz .
fi
if [[ ! -s ./UAD${mm}${dd}1200${mm}${dd}12001 ]]; then 
  cp ${UAD}/UAD${mm}${dd}1200${mm}${dd}12001.gz .
fi
if [[ ! -s ./UAD${mm}${dd}1800${mm}${dd}18001 ]]; then 
  cp ${UAD}/UAD${mm}${dd}1800${mm}${dd}18001.gz .
fi
gunzip -d *.gz


declare -i npramWnd 
let npramWnd=91*5+1

declare -a hh
declare -i ih
hh=(00 06 12 18)

ih=0
while [[ $ih -le 3 ]]
do
  echo ${yyyy}${mm}${dd}_${hh[$ih]}
  
  file_grib=UAD${mm}${dd}${hh[$ih]}00${mm}${dd}${hh[$ih]}001
  
  file_wnd_le=ecmwf_wnd${yyyy}-${mm}-${dd}.t${hh[$ih]}.le
  
  file_wnd_be=ecmwf_wnd${yyyy}-${mm}-${dd}.t${hh[$ih]}

  if [ -s ${file_wnd_be} ]; then 
    let ih=ih+1
    continue
  fi

  #---- wind ----

#echo "   ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":LSM:"  |  ${cmd1} -4yr -i ${file_grib} -nh -o ${file_wnd_le}"

   ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":LSM:"  |  ${cmd1} -4yr -i ${file_grib} -nh -o ${file_wnd_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep ":UGRD:"  | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append -bin ${file_wnd_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep ":VGRD:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin ${file_wnd_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep "master_table=5 parmcat=1 parm=83:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin  ${file_wnd_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep "master_table=5 parmcat=1 parm=84:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin  ${file_wnd_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep ":O3MR:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin ${file_wnd_le}

  #---- convert endian if necessary -----
  if [[ ${endian} -eq 0 ]] ; then
${IDL} <<EOL
!QUIET=1
.r ${COAT_PATH}/scripts/convert_endian_ecmwf.pro
convert_endian_ecmwf, "${file_wnd_le}", "${file_wnd_be}", ${npramWnd}
exit
EOL
rm -f ${file_wnd_le} 
  else
    mv -f ${file_wnd_le} ${file_wnd_be}
  fi  
  
  let ih=ih+1

done

#---- now run the surface and atmosphere dump script ----
# get_ecmwf.bash ${yyyymmdd}

rm -f ${odir}UAD*gz



