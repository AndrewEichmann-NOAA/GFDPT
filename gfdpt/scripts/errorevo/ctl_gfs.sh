#!/bin/ksh
set -x
# convert GFS 1x1 deg to 2.5 deg resolution
stmp=/stmpd2
export dir0=${1:-/global/noscrub/Fanglin.Yang/stat}      
export rundir=${2:-$stmp/$LOGNAME/vrfy_vsdb/maps/err}
export exp=${3:-gfs}      
export cyc=${4:-00}      
export tag=${5:-pgbanl}      
export sdate=${6:-01aug2007}      
export CDATE=${7:-20070831}      
export ndays=${8:-60}      
#if [ ! -s $rundir ]; then mkdir -p ${rundir}; fi
mkdir -p ${rundir}/${exp}

years=`echo $sdate |cut -c 6-9 `
mons=`echo $sdate |cut -c 3-5 `
days=`echo $sdate |cut -c 1-2 `

n=1
while [ $n -le $ndays ]; do
  hr=`expr $n \* 24 `
#  dd=`/nwprod/util/exec/ndate -$hr ${CDATE}${cyc}`
  dd=`${NDATE} -$hr ${CDATE}${cyc}`
  if [ -s ${dir0}/${exp}/${tag}.${exp}.$CDATE$cyc ] ;then
#    /nwprod/util/exec/copygb -g2 -x $dir0/${exp}/${tag}.${exp}.$CDATE$cyc ${rundir}/${exp}/${tag}.${exp}.$CDATE$cyc
    ${COPYGB} -g2 -x $dir0/${exp}/${tag}.${exp}.$CDATE$cyc ${rundir}/${exp}/${tag}.${exp}.$CDATE$cyc
  else
    tag=pgbf00
#    /nwprod/util/exec/copygb -g2 -x $dir0/${exp}/${tag}.${exp}.$CDATE$cyc ${rundir}/${exp}/${tag}.${exp}.$CDATE$cyc
    ${COPYGB} -g2 -x $dir0/${exp}/${tag}.${exp}.$CDATE$cyc ${rundir}/${exp}/${tag}.${exp}.$CDATE$cyc
  fi
n=`expr $n + 1 `
done

#-----------------------------
#datadir=${dir0}/gfs
file=${exp}.t${cyc}z.${tag}
rm ${rundir}/${file}.ctl  ${rundir}/${file}.idx
export tag1=${tag}
#if [ $tag1 = "pgbanl" ]; then tag1="pgbf00"; fi

cat >${rundir}/${file}.ctl <<EOF
dset ${rundir}/${exp}/${tag1}.${exp}.%y4%m2%d2${cyc}
format template
index ${rundir}/${file}.idx
undef 9.999E+20
dtype grib 2
options yrev
ydef 73 linear -90.000000 2.5
xdef 144 linear 0.000000 2.500000
tdef ${ndays} linear ${cyc}Z${days}${mons}${years} 1dy 
*  z has 26 levels, for pra
zdef 26 levels
1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10
vars 138
no4LFTXsfc  0 132,1,0  ** surface Best (4-layer) lifted index [K]
no5WAVA500mb  0 230,100,500 ** 500 mb 5-wave geopot. height anomaly [gpm]
no5WAVH500mb  0 222,100,500 ** 500 mb 5-wave geopotential height [gpm]
ABSVprs 26 41,100,0 ** (profile) Absolute vorticity [/s]
ACPCPsfc  0 63,1,0  ** surface Convective precipitation [kg/m^2]
ALBDOsfc  0 84,1,0  ** surface Albedo [%]
APCPsfc  0 61,1,0  ** surface Total precipitation [kg/m^2]
CAPEsfc  0 157,1,0  ** surface Convective Avail. Pot. Energy [J/kg]
CAPE180_0mb  0 157,116,46080 ** 180-0 mb above gnd Convective Avail. Pot. Energy [J/kg]
CFRZRsfc  0 141,1,0  ** surface Categorical freezing rain [yes=1;no=0]
CICEPsfc  0 142,1,0  ** surface Categorical ice pellets [yes=1;no=0]
CINsfc  0 156,1,0  ** surface Convective inhibition [J/kg]
CIN180_0mb  0 156,116,46080 ** 180-0 mb above gnd Convective inhibition [J/kg]
CLWMRprs 21 153,100,0 ** (profile) Cloud water [kg/kg]
CPRATsfc  0 214,1,0  ** surface Convective precip. rate [kg/m^2/s]
CRAINsfc  0 140,1,0  ** surface Categorical rain [yes=1;no=0]
CSNOWsfc  0 143,1,0  ** surface Categorical snow [yes=1;no=0]
CWATclm  0 76,200,0 ** atmos column Cloud water [kg/m^2]
CWORKclm  0 146,200,0 ** atmos column Cloud work function [J/kg]
DLWRFsfc  0 205,1,0  ** surface Downward long wave flux [W/m^2]
DSWRFsfc  0 204,1,0  ** surface Downward short wave flux [W/m^2]
GFLUXsfc  0 155,1,0  ** surface Ground heat flux [W/m^2]
GPAprs 2 27,100,0 ** (profile) Geopotential height anomaly [gpm]
HGTsfc  0 7,1,0  ** surface Geopotential height [gpm]
HGTprs 26 7,100,0 ** (profile) Geopotential height [gpm]
HGTpv2  0 7,117,2000 ** pot vorticity = 2000 units level Geopotential height [gpm]
HGTpvneg2  0 7,117,34768 ** pot vorticity = -2000 units level Geopotential height [gpm]
HGThtfl  0 7,204,0 ** highest trop freezing level Geopotential height [gpm]
HGT0deg  0 7,4,0 ** 0C isotherm level Geopotential height [gpm]
HGTmwl  0 7,6,0 ** max wind level Geopotential height [gpm]
HGTtrp  0 7,7,0 ** tropopause Geopotential height [gpm]
HPBLsfc  0 221,1,0  ** surface Planetary boundary layer height [m]
ICECsfc  0 91,1,0  ** surface Ice concentration (ice=1;no ice=0) [fraction]
LANDsfc  0 81,1,0  ** surface Land cover (land=1;sea=0) [fraction]
LFTXsfc  0 131,1,0  ** surface Surface lifted index [K]
LHTFLsfc  0 121,1,0  ** surface Latent heat flux [W/m^2]
O3MRprs 6 154,100,0 ** (profile) Ozone mixing ratio [kg/kg]
PEVPRsfc  0 145,1,0  ** surface Potential evaporation rate [W/m^2]
POTsig995   0 13,107,9950 ** sigma=.995  Potential temp. [K]
PRATEsfc  0 59,1,0  ** surface Precipitation rate [kg/m^2/s]
PRESsfc  0 1,1,0  ** surface Pressure [Pa]
PRESpv2  0 1,117,2000 ** pot vorticity = 2000 units level Pressure [Pa]
PRESpvneg2  0 1,117,34768 ** pot vorticity = -2000 units level Pressure [Pa]
PRESlcb  0 1,212,0 ** low cloud base Pressure [Pa]
PRESlct  0 1,213,0 ** low cloud top Pressure [Pa]
PRESmcb  0 1,222,0 ** mid-cloud base Pressure [Pa]
PRESmct  0 1,223,0 ** mid-cloud top Pressure [Pa]
PREShcb  0 1,232,0 ** high cloud base Pressure [Pa]
PREShct  0 1,233,0 ** high cloud top Pressure [Pa]
PREScvb  0 1,242,0 ** convective cld base Pressure [Pa]
PREScvt  0 1,243,0 ** convective cld top Pressure [Pa]
PRESmwl  0 1,6,0 ** max wind level Pressure [Pa]
PREStrp  0 1,7,0 ** tropopause Pressure [Pa]
PRMSLmsl  0 2,102,0  ** unknown level Pressure reduced to MSL [Pa]
PWATclm  0 54,200,0 ** atmos column Precipitable water [kg/m^2]
RHprs 21 52,100,0 ** (profile) Relative humidity [%]
RH2m  0 52,105,2 ** 2 m above ground Relative humidity [%]
RHsig995   0 52,107,9950 ** sigma=.995  Relative humidity [%]
RHsg33_100   0 52,108,8548 ** sigma=0.33-1 layer Relative humidity [%]
RHsg44_72   0 52,108,11336 ** sigma=0.44-0.72 layer Relative humidity [%]
RHsg44_100   0 52,108,11364 ** sigma=0.44-1 layer Relative humidity [%]
RHsg72_94   0 52,108,18526 ** sigma=0.72-0.94 layer Relative humidity [%]
RH30_0mb  0 52,116,7680 ** 30-0 mb above gnd Relative humidity [%]
RHclm  0 52,200,0 ** atmos column Relative humidity [%]
RHhtfl  0 52,204,0 ** highest trop freezing level Relative humidity [%]
RH0deg  0 52,4,0 ** 0C isotherm level Relative humidity [%]
SHTFLsfc  0 122,1,0  ** surface Sensible heat flux [W/m^2]
SOILW0_10cm  0 144,112,10 ** 0-10 cm underground Volumetric soil moisture [fraction]
SOILW10_40cm  0 144,112,2600 ** 10-40 cm underground Volumetric soil moisture [fraction]
SOILW40_100cm  0 144,112,10340 ** 40-100 cm underground Volumetric soil moisture [fraction]
SOILW100_200cm  0 144,112,25800 ** 100-200 cm underground Volumetric soil moisture [fraction]
SPFH2m  0 51,105,2 ** 2 m above ground Specific humidity [kg/kg]
SPFH30_0mb  0 51,116,7680 ** 30-0 mb above gnd Specific humidity [kg/kg]
TCDCclm  0 71,200,0 ** atmos column Total cloud cover [%]
TCDCbcl  0 71,211,0 ** boundary cld layer Total cloud cover [%]
TCDClcl  0 71,214,0 ** low cloud level Total cloud cover [%]
TCDCmcl  0 71,224,0 ** mid-cloud level Total cloud cover [%]
TCDChcl  0 71,234,0 ** high cloud level Total cloud cover [%]
TCDCcvl  0 71,244,0 ** convective cld layer Total cloud cover [%]
TMAX2m  0 15,105,2 ** 2 m above ground Max. temp. [K]
TMIN2m  0 16,105,2 ** 2 m above ground Min. temp. [K]
TMPsfc  0 11,1,0  ** surface Temp. [K]
TMPprs 26 11,100,0 ** (profile) Temp. [K]
TMP3658m  0 11,103,3658 ** 3658 m above msl Temp. [K]
TMP2743m  0 11,103,2743 ** 2743 m above msl Temp. [K]
TMP1829m  0 11,103,1829 ** 1829 m above msl Temp. [K]
TMP2m  0 11,105,2 ** 2 m above ground Temp. [K]
TMPsig995   0 11,107,9950 ** sigma=.995  Temp. [K]
TMP0_10cm  0 11,112,10 ** 0-10 cm underground Temp. [K]
TMP10_40cm  0 11,112,2600 ** 10-40 cm underground Temp. [K]
TMP40_100cm  0 11,112,10340 ** 40-100 cm underground Temp. [K]
TMP100_200cm  0 11,112,25800 ** 100-200 cm underground Temp. [K]
TMP30_0mb  0 11,116,7680 ** 30-0 mb above gnd Temp. [K]
TMPpv2  0 11,117,2000 ** pot vorticity = 2000 units level Temp. [K]
TMPpvneg2  0 11,117,34768 ** pot vorticity = -2000 units level Temp. [K]
TMPlct  0 11,213,0 ** low cloud top Temp. [K]
TMPmct  0 11,223,0 ** mid-cloud top Temp. [K]
TMPhct  0 11,233,0 ** high cloud top Temp. [K]
TMPmwl  0 11,6,0 ** max wind level Temp. [K]
TMPtrp  0 11,7,0 ** tropopause Temp. [K]
TOZNEclm  0 10,200,0 ** atmos column Total ozone [Dobson]
UGWDsfc  0 147,1,0  ** surface Zonal gravity wave stress [N/m^2]
UFLXsfc  0 124,1,0  ** surface Zonal momentum flux [N/m^2]
UGRDprs 26 33,100,0 ** (profile) u wind [m/s]
UGRD3658m  0 33,103,3658 ** 3658 m above msl u wind [m/s]
UGRD2743m  0 33,103,2743 ** 2743 m above msl u wind [m/s]
UGRD1829m  0 33,103,1829 ** 1829 m above msl u wind [m/s]
UGRD10m  0 33,105,10 ** 10 m above ground u wind [m/s]
UGRDsig995   0 33,107,9950 ** sigma=.995  u wind [m/s]
UGRD30_0mb  0 33,116,7680 ** 30-0 mb above gnd u wind [m/s]
UGRDpv2  0 33,117,2000 ** pot vorticity = 2000 units level u wind [m/s]
UGRDpvneg2  0 33,117,34768 ** pot vorticity = -2000 units level u wind [m/s]
UGRDmwl  0 33,6,0 ** max wind level u wind [m/s]
UGRDtrp  0 33,7,0 ** tropopause u wind [m/s]
ULWRFsfc  0 212,1,0  ** surface Upward long wave flux [W/m^2]
ULWRFtoa  0 212,8,0 ** top of atmos Upward long wave flux [W/m^2]
USWRFsfc  0 211,1,0  ** surface Upward short wave flux [W/m^2]
USWRFtoa  0 211,8,0 ** top of atmos Upward short wave flux [W/m^2]
VGWDsfc  0 148,1,0  ** surface Meridional gravity wave stress [N/m^2]
VFLXsfc  0 125,1,0  ** surface Meridional momentum flux [N/m^2]
VGRDprs 26 34,100,0 ** (profile) v wind [m/s]
VGRD3658m  0 34,103,3658 ** 3658 m above msl v wind [m/s]
VGRD2743m  0 34,103,2743 ** 2743 m above msl v wind [m/s]
VGRD1829m  0 34,103,1829 ** 1829 m above msl v wind [m/s]
VGRD10m  0 34,105,10 ** 10 m above ground v wind [m/s]
VGRDsig995   0 34,107,9950 ** sigma=.995  v wind [m/s]
VGRD30_0mb  0 34,116,7680 ** 30-0 mb above gnd v wind [m/s]
VGRDpv2  0 34,117,2000 ** pot vorticity = 2000 units level v wind [m/s]
VGRDpvneg2  0 34,117,34768 ** pot vorticity = -2000 units level v wind [m/s]
VGRDmwl  0 34,6,0 ** max wind level v wind [m/s]
VGRDtrp  0 34,7,0 ** tropopause v wind [m/s]
VVELprs 21 39,100,0 ** (profile) Pressure vertical velocity [Pa/s]
VVELsig995   0 39,107,9950 ** sigma=.995  Pressure vertical velocity [Pa/s]
VWSHpv2  0 136,117,2000 ** pot vorticity = 2000 units level Vertical speed shear [1/s]
VWSHpvneg2  0 136,117,34768 ** pot vorticity = -2000 units level Vertical speed shear [1/s]
VWSHtrp  0 136,7,0 ** tropopause Vertical speed shear [1/s]
WATRsfc  0 90,1,0  ** surface Water runoff [kg/m^2]
WEASDsfc  0 65,1,0  ** surface Accum. snow [kg/m^2]
ENDVARS
EOF

gribmap -0 -i ${rundir}/${file}.ctl
exit
