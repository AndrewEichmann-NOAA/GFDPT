#!/bin/sh
set -x

source ~/.bashrc
#module load python
module load python/2.7.12

export TARFILENAME=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${spdy}/gdas.t00z.radstat
cd $RADDATADIR/radstat

mkdir ${spdy}00
cd ${spdy}00

#for INSTRUMENT in $DATASOURCES
#do

#  export DIAGFILENAME=diag_${INSTRUMENT}_anl.${spdy}00.gz
#  rm $DIAGFILENAME
#  tar xvf $TARFILENAME $DIAGFILENAME
#  gunzip -f $DIAGFILENAME

#  export DIAGFILENAME=diag_${INSTRUMENT}_ges.${spdy}00.gz
#  rm $DIAGFILENAME
#  tar xvf $TARFILENAME $DIAGFILENAME
#  gunzip -f $DIAGFILENAME

#done

python $RADSCRIPTDIR/getdiagfiles.py 0

cd $RADDATADIR/plotrad


for INSTRUMENT in $DATASOURCES
do

python $RADSCRIPTDIR/makeradplots.py ${spdy}00 $INSTRUMENT >& plotrad.${spdy}00.${INSTRUMENT} &
sleep 1m # to let first instance create directory w/o race condition

done 

#python $RADSCRIPTDIR/makeradplots.py ${spdy}00 'atms_npp' >& plotrad.${spdy}00.atms_npp &
#sleep 1m # to let first instance create directory w/o race condition
#python $RADSCRIPTDIR/makeradplots.py ${spdy}00 'amsua_metop-a' >& plotrad.${spdy}00.amsua_metop-a &
#python $RADSCRIPTDIR/makeradplots.py ${spdy}00 'amsua_metop-b' >& plotrad.${spdy}00.amsua_metop-b &
#python $RADSCRIPTDIR/makeradplots.py ${spdy}00 'amsua_n15' >& plotrad.${spdy}00.amsua_n15 &
#python $RADSCRIPTDIR/makeradplots.py ${spdy}00 'amsua_n18' >& plotrad.${spdy}00.amsua_n18 &
#python $RADSCRIPTDIR/makeradplots.py ${spdy}00 'amsua_n19' >& plotrad.${spdy}00.amsua_n19 &

sleep 70m

cd $RADDATADIR/plotrad/${spdy}00


for i in `ls` ; do
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




scp  -rv $RADDATADIR/plotrad/${spdy}00  $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/plotrad/
ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${spdy}00 >> $REMOTEWEBDIR/plotrad/allow.cfg"

