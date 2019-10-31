#!/bin/ksh
set -xaeu
#run=para
run=prod
indir=/ptmpd2/$LOGNAME/${run}gribs
gribinv=/nwprod/util/exec/wgrib
gribout=/ptmpd2/$LOGNAME/gribext.$run.gfsecmwf
mkdir -p $gribout
export idim=360
export jdim=181
days=20150921 
daye=20150921
day=$days
yr=`echo $day | cut -c 1-4`

until [ $day -gt $daye ] ; do
#for hh in 00 06 12 18
for hh in  00
do
echo "day hh Year $day $hh $yr"
grep "$day $hh" /com2/arch/prod/syndat/syndat_tcvitals.$yr | sort | cut -c 20-44,63-66 | uniq > $gribout/tcv.$day$hh
size=`ls -l $gribout/tcv.$day$hh | awk '{print $5}'`
echo "Size $size"
if [ $size -eq 0 ] ; then
echo "$day $hh NONE" > $gribout/tcv.$day$hh
fi

export nlev=14
export igau=0
export input_ges=$indir/grib.ges.$day$hh
#export input_gfs=$indir/grib.gdas.anl.$day$hh
export input_gfs=$indir/grib.gfs.anl.$day$hh
export input_ecmwf=$indir/pgbanl.$day$hh
export tropl=$gribout/tcv.$day$hh
if [ -s $input_ges ] ; then
if [ -s $input_gfs ] ; then
if [ -s $input_ecmwf ] ; then
ln -sf  "$gribout/gemout.$day$hh"                                   "fort.55"
ln -sf  "$gribout/gradsout.$day$hh"                                 "fort.60"
ln -sf  "$gribout/gribext.trops.$day$hh"                            "fort.65"
ln -sf  "$gribout/gribext.nontrops.$day$hh"                         "fort.66"
./gfsecmwfgrib.x > $gribout/rdgrib.$run.gfs.ec.ges.$day$hh <$tropl 2> $gribout/rdgrib_outout.$run.$day$hh
#
if [ -s $gribout/gribext.trops.$day$hh ] ; then
cat $gribout/gribext.trops.$day$hh >> $gribout/gribhist.gfs.ecmwf.trops
fi
if [ -s $gribout/gribext.nontrops.$day$hh ] ; then
cat $gribout/gribext.nontrops.$day$hh >> $gribout/gribhist.gfs.ecmwf.nontrops
fi
#
fi
fi
fi
done

day=`expr $day + 1`
done
