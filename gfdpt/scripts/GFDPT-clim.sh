#!/bin/sh
set -x

source ~/.bashrc
#module load python/2.7.12

cd $RADDATADIR/radstat




cd $RADDATADIR/clim


for INSTRUMENT in $DATASOURCES
do

#python $RADSCRIPTDIR/makeradclim.py ${spdy}00 $INSTRUMENT >& plotrad.${spdy}00.$INSTRUMENT
python $RADSCRIPTDIR/makeradclim.py ${spdycyc} $INSTRUMENT >& plotrad.${spdycyc}.$INSTRUMENT

done


cd ${spdycyc}

for i in `$LS` ; do
 if [ -d  $i ]; then
   cd $i
   pwd
   $LS > allow.cfg
   cp $TEMPLATESDIR/index.php .
   cd ..
 fi
done;

$LS > allow.cfg
cp $TEMPLATESDIR/index.php .




scp  -rv $RADDATADIR/clim/${spdycyc}  $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/clim/
ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${spdycyc} >> $REMOTEWEBDIR/clim/allow.cfg"

