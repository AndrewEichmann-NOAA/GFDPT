#!/bin/sh
set -x

source ~/.bashrc
module unload ics
module load ics/13.1

export CDATE=`date -d'5 days ago' +%Y%m%d`
export edate=`date -d$CDATE +%d%b%Y`
export explist="gfs ecm"
export fhrlist="f00 f06 f12 f18 f24 f30 f36 f42 f48 f54 f60 f66 f72 f78 f84 f90 f96 f102 f108 f114 f120"
export cyc="00"
export arealist="sh"

#cd /global/save/Andrew.Eichmann/dropout
cd $GFDPTSCRIPTDIR/errorevo

./maps2d_fcstdiff3.sh  $CDATE $arealist  $cyc  "f00 f06 f12 f18 f24 f30 f36 f42 f48 f54 f60 f66 f72 f78 f84 f90 f96 f102 f108 f114 f120" "gfs ecm" $GFDPTDATADIR/errorevo/$CDATE/fcstdiffSH

export arealist="nh"
./maps2d_fcstdiff3.sh  $CDATE $arealist  $cyc  "f00 f06 f12 f18 f24 f30 f36 f42 f48 f54 f60 f66 f72 f78 f84 f90 f96 f102 f108 f114 f120" "gfs ecm" $GFDPTDATADIR/errorevo/$CDATE/fcstdiffNH

mkdir $WEBSTAGEDIR/errorevo/${CDATE}00

#cp $GFDPTDATADIR/errorevo/$CDATE/fcstdiff[SN]H//HGTprs*_anim_t00z.gif $WEBSTAGEDIR/errorevo/${CDATE}00
cp $GFDPTDATADIR/errorevo/$CDATE/fcstdiff[SN]H//HGTprs*.gif $WEBSTAGEDIR/errorevo/${CDATE}00

cd $WEBSTAGEDIR/errorevo/${CDATE}00
$LS  > allow.cfg
cp $TEMPLATESDIR/index.php .

scp -r $WEBSTAGEDIR/errorevo/${CDATE}00 $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/errorevo

ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${CDATE}00 >> $REMOTEWEBDIR/errorevo/allow.cfg"
