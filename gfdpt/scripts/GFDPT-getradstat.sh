#!/bin/sh
set -x

source ~/.bashrc
#module load python
module load python/2.7.12

#export cyc=00

#export TARFILENAME=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${spdy}/gdas.t00z.radstat
export TARFILENAME=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${spdy}/gdas.t${cyc}z.radstat
cd $RADDATADIR/radstat

mkdir ${spdy}${cyc}
cd ${spdy}${cyc}

python $RADSCRIPTDIR/getdiagfiles.py 0

########################
exit
#######################

cd $RADDATADIR/plotrad


for INSTRUMENT in $DATASOURCES
do

python $RADSCRIPTDIR/makeradplots.py ${spdy}${cyc} $INSTRUMENT >& plotrad.${spdy}${cyc}.${INSTRUMENT} &
sleep 1m # to let first instance create directory w/o race condition

done 

sleep 70m

cd $RADDATADIR/plotrad/${spdy}${cyc}


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




scp  -rv $RADDATADIR/plotrad/${spdy}${cyc}  $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/plotrad/
ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${spdy}${cyc} >> $REMOTEWEBDIR/plotrad/allow.cfg"

