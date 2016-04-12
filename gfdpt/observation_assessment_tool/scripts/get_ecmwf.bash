#! /bin/bash
########################################################################
# Puopose: 
#	To replace the original Perl Script written by Peter Keehn.
#	To eliminate inter-mediate files and the cat command usage,
#	instead, use wgrib -append option.
# 
# Updated: Extract both grib1 and grib2 data.
#          ECMWF data is updated to mixed grib1 and grib2 since
#          18 May 2011.   2011-05-18 00 hour file is still grib1 fomat,
#          2011-05-18 06 hour file changed to grib1 & grib2 mixed format.  
#
# Author:  
#	Wanchun Chen  NOAA/NESDIS/STAR @ PSGS Coompany
#
# Ref:
#  http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/convert_wgrib2.html   
#  
# Date:
#	06/02/2011
#
# Usg: get_ecmwf.bash yyyymmdd
# Exp: get_ecmwf.bash 20110530
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


cd ${NWP_DATA_PATH}/ecmwf/

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
#cp ${UAD}/UAD${mm}${dd}0000${mm}${dd}00001.gz .
#cp ${UAD}/UAD${mm}${dd}0600${mm}${dd}06001.gz .
#cp ${UAD}/UAD${mm}${dd}1200${mm}${dd}12001.gz .
#cp ${UAD}/UAD${mm}${dd}1800${mm}${dd}18001.gz .
gunzip -d *.gz

declare -i npramAtm npramSfc
let npramAtm=91*5+1
let npramSfc=6

declare -a hh
declare -i ih
hh=(00 06 12 18)

ih=0
while [[ $ih -le 3 ]]
do
  echo ${yyyy}${mm}${dd}_${hh[$ih]}
  
  file_grib=UAD${mm}${dd}${hh[$ih]}00${mm}${dd}${hh[$ih]}001
  
  file_sfc_le=ecmwf_sfc${yyyy}-${mm}-${dd}.t${hh[$ih]}.le
  file_atm_le=ecmwf_atm${yyyy}-${mm}-${dd}.t${hh[$ih]}.le
  
  file_sfc_be=ecmwf_sfc${yyyy}-${mm}-${dd}.t${hh[$ih]}
  file_atm_be=ecmwf_atm${yyyy}-${mm}-${dd}.t${hh[$ih]}
  if [ -s ${file_sfc_be} ]; then 
    let ih=ih+1
#    continue
  fi
  #---- sfc ----
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":LSM:"  |  ${cmd1} -4yr -i ${file_grib} -nh         -o ${file_sfc_le}
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":SP:"   |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":SKT:"  |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":2T:"   |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":10U:"  |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":10V:"  |  ${cmd1} -4yr -i ${file_grib} -nh -append -o ${file_sfc_le}
 
  #---- atm ----
  ${cmd1}  -s  ${file_grib} 2> /dev/null | grep ":LSM:"  |  ${cmd1} -4yr -i ${file_grib} -nh -o ${file_atm_le}
  
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep ":TMP:"  | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin ${file_atm_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep ":SPFH:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin ${file_atm_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep "master_table=5 parmcat=1 parm=83:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin  ${file_atm_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep "master_table=5 parmcat=1 parm=84:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin  ${file_atm_le}
  ${cmd2}  -s  ${file_grib} 2> /dev/null | grep ":O3MR:" | sort -t: -n -k5  |  ${cmd2} -i ${file_grib} -no_header -order we:ns -append  -bin ${file_atm_le}
  
  #---- convert endian if necessary -----
  if [[ ${endian} -eq 0 ]] ; then
${IDL} <<EOL
!QUIET=1
.r ${COAT_PATH}/scripts/convert_endian_ecmwf.pro
convert_endian_ecmwf, "${file_atm_le}", "${file_atm_be}", ${npramAtm}
convert_endian_ecmwf, "${file_sfc_le}", "${file_sfc_be}", ${npramSfc}
exit
EOL
rm -f ${file_sfc_le} ${file_atm_le}
  else
    mv -f ${file_sfc_le} ${file_sfc_be}
    mv -f ${file_atm_le} ${file_atm_be}
  fi  
  
  
  let ih=ih+1

done

#rm -f /home/pub/external_data/ecmwf/UAD*
