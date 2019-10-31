#!/bin/sh

set -xa

export HH=00


cd $WEBSTAGEDIR/extdiff/$spdy$HH
for file in `ls gfs*gif ecmwf*gif`
do
   convert $file  -resize 30% thumbnail.$file
done






