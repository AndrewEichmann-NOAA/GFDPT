#!/bin/sh
set -x


if [ "$cyc" -eq '12' ]; then
#AFEauto  export spdycyc=`/nwprod/util/exec/ndate -24 $(date +%Y%m%d)$cyc`
  export spdycyc=`/nwprod/util/exec/ndate -24 $spdy$cyc`
  export spdy=`echo $spdycyc | cut -c1-8`
fi



export inputdir=$FFCORRDIR/${spdy}${cyc}/mapfiles

#export savdir=$inputdir
#ls -d $savdir
#  err0=$?
#  if test $err0 -eq 0
#  then
#  ls
#  else
#  echo "$savdir directory does not exist"
#  echo "Safe to create these"
#  fi

cd $inputdir

#  mkdir -p $savdir
##
   ln -s  $inputdir/meancor_HGT_P1000_G2NHX*.txt  ./meancor_HGT_P1000_G2NHX.txt
   ln -s  $inputdir/meancor_HGT_P250_G2NHX*.txt  ./meancor_HGT_P250_G2NHX.txt
   ln -s  $inputdir/meancor_HGT_P500_G2NHX*.txt  ./meancor_HGT_P500_G2NHX.txt
   ln -s  $inputdir/meancor_HGT_P700_G2NHX*.txt  ./meancor_HGT_P700_G2NHX.txt
#
   ln -s  $inputdir/meancor_T_P250_G2NHX*.txt  ./meancor_T_P250_G2NHX.txt
   ln -s  $inputdir/meancor_T_P500_G2NHX*.txt  ./meancor_T_P500_G2NHX.txt
   ln -s  $inputdir/meancor_T_P850_G2NHX*.txt  ./meancor_T_P850_G2NHX.txt
#
   ln -s  $inputdir/meancor_U_P250_G2NHX*.txt  ./meancor_U_P250_G2NHX.txt
   ln -s  $inputdir/meancor_U_P500_G2NHX*.txt  ./meancor_U_P500_G2NHX.txt
   ln -s  $inputdir/meancor_U_P850_G2NHX*.txt  ./meancor_U_P850_G2NHX.txt
#
   ln -s  $inputdir/meancor_V_P250_G2NHX*.txt  ./meancor_V_P250_G2NHX.txt
   ln -s  $inputdir/meancor_V_P500_G2NHX*.txt  ./meancor_V_P500_G2NHX.txt
   ln -s  $inputdir/meancor_V_P850_G2NHX*.txt  ./meancor_V_P850_G2NHX.txt
#
   ln -s  $inputdir/meancor_WIND_P250_G2NHX*.txt  ./meancor_WIND_P250_G2NHX.txt
   ln -s  $inputdir/meancor_WIND_P500_G2NHX*.txt  ./meancor_WIND_P500_G2NHX.txt
   ln -s  $inputdir/meancor_WIND_P850_G2NHX*.txt  ./meancor_WIND_P850_G2NHX.txt
##
   ln -s  $inputdir/meancor_HGT_P1000_G2SHX*.txt  ./meancor_HGT_P1000_G2SHX.txt
   ln -s  $inputdir/meancor_HGT_P250_G2SHX*.txt  ./meancor_HGT_P250_G2SHX.txt
   ln -s  $inputdir/meancor_HGT_P500_G2SHX*.txt  ./meancor_HGT_P500_G2SHX.txt
   ln -s  $inputdir/meancor_HGT_P700_G2SHX*.txt  ./meancor_HGT_P700_G2SHX.txt
#
   ln -s  $inputdir/meancor_T_P250_G2SHX*.txt  ./meancor_T_P250_G2SHX.txt
   ln -s  $inputdir/meancor_T_P500_G2SHX*.txt  ./meancor_T_P500_G2SHX.txt
   ln -s  $inputdir/meancor_T_P850_G2SHX*.txt  ./meancor_T_P850_G2SHX.txt
#
   ln -s  $inputdir/meancor_U_P250_G2SHX*.txt  ./meancor_U_P250_G2SHX.txt
   ln -s  $inputdir/meancor_U_P500_G2SHX*.txt  ./meancor_U_P500_G2SHX.txt
   ln -s  $inputdir/meancor_U_P850_G2SHX*.txt  ./meancor_U_P850_G2SHX.txt
#
   ln -s  $inputdir/meancor_V_P250_G2SHX*.txt  ./meancor_V_P250_G2SHX.txt
   ln -s  $inputdir/meancor_V_P500_G2SHX*.txt  ./meancor_V_P500_G2SHX.txt
   ln -s  $inputdir/meancor_V_P850_G2SHX*.txt  ./meancor_V_P850_G2SHX.txt
#
   ln -s  $inputdir/meancor_WIND_P250_G2SHX*.txt  ./meancor_WIND_P250_G2SHX.txt
   ln -s  $inputdir/meancor_WIND_P500_G2SHX*.txt  ./meancor_WIND_P500_G2SHX.txt
   ln -s  $inputdir/meancor_WIND_P850_G2SHX*.txt  ./meancor_WIND_P850_G2SHX.txt

   ln -s  $inputdir/meanrms_HGT_P1000_G2NHX*.txt  ./meanrms_HGT_P1000_G2NHX.txt
   ln -s  $inputdir/meanrms_HGT_P250_G2NHX*.txt  ./meanrms_HGT_P250_G2NHX.txt
   ln -s  $inputdir/meanrms_HGT_P500_G2NHX*.txt  ./meanrms_HGT_P500_G2NHX.txt
   ln -s  $inputdir/meanrms_HGT_P700_G2NHX*.txt  ./meanrms_HGT_P700_G2NHX.txt
#
   ln -s  $inputdir/meanrms_T_P250_G2NHX*.txt  ./meanrms_T_P250_G2NHX.txt
   ln -s  $inputdir/meanrms_T_P500_G2NHX*.txt  ./meanrms_T_P500_G2NHX.txt
   ln -s  $inputdir/meanrms_T_P850_G2NHX*.txt  ./meanrms_T_P850_G2NHX.txt
#
   ln -s  $inputdir/meanrms_U_P250_G2NHX*.txt  ./meanrms_U_P250_G2NHX.txt
   ln -s  $inputdir/meanrms_U_P500_G2NHX*.txt  ./meanrms_U_P500_G2NHX.txt
   ln -s  $inputdir/meanrms_U_P850_G2NHX*.txt  ./meanrms_U_P850_G2NHX.txt
#
   ln -s  $inputdir/meanrms_V_P250_G2NHX*.txt  ./meanrms_V_P250_G2NHX.txt
   ln -s  $inputdir/meanrms_V_P500_G2NHX*.txt  ./meanrms_V_P500_G2NHX.txt
   ln -s  $inputdir/meanrms_V_P850_G2NHX*.txt  ./meanrms_V_P850_G2NHX.txt
#
   ln -s  $inputdir/meanrms_WIND_P250_G2NHX*.txt  ./meanrms_WIND_P250_G2NHX.txt
   ln -s  $inputdir/meanrms_WIND_P500_G2NHX*.txt  ./meanrms_WIND_P500_G2NHX.txt
   ln -s  $inputdir/meanrms_WIND_P850_G2NHX*.txt  ./meanrms_WIND_P850_G2NHX.txt
##
   ln -s  $inputdir/meanrms_HGT_P1000_G2SHX*.txt  ./meanrms_HGT_P1000_G2SHX.txt
   ln -s  $inputdir/meanrms_HGT_P250_G2SHX*.txt  ./meanrms_HGT_P250_G2SHX.txt
   ln -s  $inputdir/meanrms_HGT_P500_G2SHX*.txt  ./meanrms_HGT_P500_G2SHX.txt
   ln -s  $inputdir/meanrms_HGT_P700_G2SHX*.txt  ./meanrms_HGT_P700_G2SHX.txt
#
   ln -s  $inputdir/meanrms_T_P250_G2SHX*.txt  ./meanrms_T_P250_G2SHX.txt
   ln -s  $inputdir/meanrms_T_P500_G2SHX*.txt  ./meanrms_T_P500_G2SHX.txt
   ln -s  $inputdir/meanrms_T_P850_G2SHX*.txt  ./meanrms_T_P850_G2SHX.txt
#
   ln -s  $inputdir/meanrms_U_P250_G2SHX*.txt  ./meanrms_U_P250_G2SHX.txt
   ln -s  $inputdir/meanrms_U_P500_G2SHX*.txt  ./meanrms_U_P500_G2SHX.txt
   ln -s  $inputdir/meanrms_U_P850_G2SHX*.txt  ./meanrms_U_P850_G2SHX.txt
#
   ln -s  $inputdir/meanrms_V_P250_G2SHX*.txt  ./meanrms_V_P250_G2SHX.txt
   ln -s  $inputdir/meanrms_V_P500_G2SHX*.txt  ./meanrms_V_P500_G2SHX.txt
   ln -s  $inputdir/meanrms_V_P850_G2SHX*.txt  ./meanrms_V_P850_G2SHX.txt
#
   ln -s  $inputdir/meanrms_WIND_P250_G2SHX*.txt  ./meanrms_WIND_P250_G2SHX.txt
   ln -s  $inputdir/meanrms_WIND_P500_G2SHX*.txt  ./meanrms_WIND_P500_G2SHX.txt
   ln -s  $inputdir/meanrms_WIND_P850_G2SHX*.txt  ./meanrms_WIND_P850_G2SHX.txt

#cd -

