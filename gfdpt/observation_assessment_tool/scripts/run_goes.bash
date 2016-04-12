#!/bin/bash

SID=53
for GID in 13 15; do 
  for DID in 1 2 3 4; do 
    SID=$((SID + 1))
    echo $GID $SID $DID
    sed -e "s|SID|${SID}|" -e "s|DID|${DID}|" -e "s|GID|${GID}|" goes_sndr.setup > instrument.setup
    run_colocfwd.bash goes${GID}_sndrd${DID} 20141005
#    exit 0
   done
done

