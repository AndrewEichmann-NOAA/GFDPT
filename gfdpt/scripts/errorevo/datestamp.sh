#!/bin/ksh

indate=${1:-20090101}     ;#e.g 20070101 
nday=${2:-0}       ;# days to add or extract
cyc=`echo $indate |cut -c9-10`
ct=`echo $indate | wc -c`
if [ $ct -ge 10 ]; then
indate=`echo $indate |cut -c 1-8 `
fi
nhours=`expr $nday \* 24`
if [ $nhours -le 0 ]; then
# INDATE=`/nwprod/util/exec/ndate $nhours ${indate}00`
 INDATE=`${NDATE} $nhours ${indate}00`
else
# INDATE=`/nwprod/util/exec/ndate +$nhours ${indate}00`
 INDATE=`${NDATE} +$nhours ${indate}00`
fi

yy=`echo $INDATE |cut -c 1-4 `
mm=`echo $INDATE |cut -c 5-6 `
dd=`echo $INDATE |cut -c 7-8 `

set -A mlist none Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
if [ $ct -ge 10 ]; then
odate=${cyc}Z${dd}${mlist[$mm]}${yy} 
echo $odate
else
outdate=${dd}${mlist[$mm]}${yy}
echo $outdate
fi
exit
