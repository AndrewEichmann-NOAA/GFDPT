#!/bin/sh
set -x

source ~/.bashrc
#module load python/2.7.12

$GFDPTSCRIPTDIR/website/generate_extanl_pages.py > $GFDPTTMPDIR/ext_anl_tmp
cat $TEMPLATESDIR/template_extanldiff_tab.php > $WEBSTAGEDIR/extanldiff_tab.php
cat $GFDPTTMPDIR/ext_anl_tmp >> $WEBSTAGEDIR/extanldiff_tab.php


#cd $WEBSTAGEDIR/extdiff/${spdy}00
cd $WEBSTAGEDIR/extdiff/${spdycyc}
for file in `$LS gfs*gif ecmwf*gif`

do
   convert $file  -resize 30% thumbnail.$file
done

#python $GFDPTSCRIPTDIR/website/generate_extanl_pages_map.py ${spdy}00 > imgmap.${spdy}00
python $GFDPTSCRIPTDIR/website/generate_extanl_pages_map.py ${spdycyc} > imgmap.${spdycyc}


#scp imgmap.${spdy}00 $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/extdiffimg.php
scp imgmap.${spdycyc} $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/extdiffimg.php

$LS > allow.cfg
cp $TEMPLATESDIR/index.php .


#cd $WEBSTAGEDIR 
cd $WEBSTAGEDIR/extdiff

#scp -i ~/.ssh/id_rsa  -r extdiff/${spdycyc} $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/extdiff
scp -i ~/.ssh/id_rsa  -r ${spdycyc} $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR/extdiff
scp -i ~/.ssh/id_rsa ../extanldiff_tab.php  $REMOTEWEBUSER@$REMOTEWEBSERVER:$REMOTEWEBDIR

#ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${spdy}00 >> $REMOTEWEBDIR/extdiff/allow.cfg"
ssh $REMOTEWEBUSER@$REMOTEWEBSERVER "echo ${spdycyc} >> $REMOTEWEBDIR/extdiff/allow.cfg"

