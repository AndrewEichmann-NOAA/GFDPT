#!/bin/sh
set -x

export WEBSCRIPTDIR=$GFDPTSCRIPTDIR/website

mkdir $WEBSTAGEDIR/ffcorr/$spdy
cd $WEBSTAGEDIR/ffcorr/$spdy
ln -s $FFCORRDIR/plots/$spdy/* .
$LS > allow.cfg
cp $TEMPLATESDIR/index.php .


python $WEBSCRIPTDIR/makeimgpages.py
sh $WEBSCRIPTDIR/makeffcorrthumbnails.sh
sh $WEBSCRIPTDIR/fcstdiv_web_update.sh

cd $WEBSTAGEDIR

scp  -i ~/.ssh/id_rsa -r ffcorr/$spdy $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/ffcorr
scp -i ~/.ssh/id_rsa  charts/* $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/charts
scp -i ~/.ssh/id_rsa *.php date.txt $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR

ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${spdy} >> $REMOTEWEBDIR/ffcorr/allow.cfg"


