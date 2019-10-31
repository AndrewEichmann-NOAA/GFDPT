#!/bin/sh
set -x

#----------------------------------------------------------------------
#export cyc=00
#export STMP=/stmpd3
export STMP=/gpfs/hps3/stmp/
#NDATE=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate

if [ "$cyc" -eq '12' ]; then
  export spdycyc=`$NDATE -24 $spdy$cyc`
  export spdy=`echo $spdycyc | cut -c1-8`
fi

export tmpdir=$STMP/$LOGNAME/nwpvrfy${spdy}${cyc}               ;#temporary directory for running verification
export savdir=$FFCORRDIR/${spdy}${cyc}/mapfiles
ls -d $savdir
  err0=$?
  if test $err0 -eq 0
  then
   rm -rf $savdir
  ls
  else
  echo "$savdir directory does not exist"
  echo "Safe to create these"
  fi
  mkdir -p $savdir
##

regions='G2 G2NHX G2PNA G2SHX G2TRO'
for reg in $regions
do

vsdbfields='anom pres'
for vsdbf in $vsdbfields
do

 if [ $vsdbf = anom ]; then
   var_anom='HGT HGTWV0-3 HGTWV10-20 HGTWV4-9 PMSL T U V WIND'
   for vara in $var_anom
   do

   cp $tmpdir/acrms*/$reg/$vsdbf/$vara/*gs $savdir/.
   cp $tmpdir/acrms*/$reg/$vsdbf/$vara/*txt $savdir/.
   cp $tmpdir/acrms*/$reg/$vsdbf/$vara/*bin $savdir/.
   cp $tmpdir/acrms*/$reg/$vsdbf/$vara/*ctl $savdir/.
   done
 else
    var_pres='HGT O3 T U V WIND'
    for varp in $var_pres
    do

    cp $tmpdir/acrms*/$reg/$vsdbf/$varp/rms*gs $savdir/.
    cp $tmpdir/acrms*/$reg/$vsdbf/$varp/*txt $savdir/.
    cp $tmpdir/acrms*/$reg/$vsdbf/$varp/*bin $savdir/.
    cp $tmpdir/acrms*/$reg/$vsdbf/$varp/*ctl $savdir/.
       
    done
 fi

done
        
done

