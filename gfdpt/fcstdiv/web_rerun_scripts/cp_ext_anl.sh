#!/bin/sh

set -xa

## Set today's date.

export PDY=`date "+%Y%m%d"`00
echo $PDY

## Set input and output directories.

export INDIR=/ptmp/wx12ss/f-f-divergence/gfs_ecmwf_gribextremes_$PDY
export OUTDIR=/ptmp/wx12ss/fcstdiv_extremes

cp /nco/pmb/wx12ss/fcstdiv/extanl_convert.sh $OUTDIR/.

## Use the gemout.$PDY file to get all necessary attributes 
## for each individual case.  These attributes include:  case 
## number (column 1), ID (column 2), latitude (column 3), 
## longitude (column 4), and priority(column 8).

export casefile=$INDIR/gemout.$PDY

## On the ncointra server, create the directories for this run's
## Extreme Analysis Cases.

ssh -l sdm ncointra "mkdir -p /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY | \
mkdir -p /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/high | \
mkdir -p /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/low"

## Copy the base files to the RZDM servers (WWB and NCWCP).

scp $INDIR/gemout.* $INDIR/gradsout.* sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/.

## Create a general list of all of the gif files for
## today's cases.  Also, create a formatted list for
## parsing out case number information.

ls $INDIR/*_$PDY_*.gif > $OUTDIR/giffilelist
ls $INDIR/*_$PDY_*.gif | sed -e "s/\/ptmp\/wx12ss\/gfs_ecmwf_gribextremes_$PDY\///g" -e "s/_hgt_/ hgt /g" -e "s/_tmp_/ tmp /g" > $OUTDIR/giflist

echo $?

## While loop to define the variables for each case and
## copy the appropriate files to the specific directories
## on the RZDM.

exec 6<$casefile
while read -u6 line; do
   echo $line > $OUTDIR/casefile
   caseid=`awk '{ printf "%4s_%3s_%4s", $2, $3, $4 }' $OUTDIR/casefile > $OUTDIR/caseid.out`
   sed -e 's/ //g' $OUTDIR/caseid.out > $OUTDIR/caseid
   id=`cat $OUTDIR/caseid`
   
   priority=`awk '{ print $8 }' $OUTDIR/casefile`

   case $priority in
      1 ) ssh -l sdm ncointra "mkdir -p /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/high/$id" 
          echo $?
          ;;
      2 ) ssh -l sdm ncointra "mkdir -p /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/low/$id" 
          echo $?
          ;;
    esac

    casenumber=`awk '{ print $1 }' $OUTDIR/casefile`
    idparam=`awk '{ print $2 }' $OUTDIR/casefile | cut -c1`

      for gifcase in `awk '{ print $3 }' $OUTDIR/giflist`
      do
         if [[ "${gifcase%%_*}" == "$casenumber" ]]
         then
            scpfile=`grep $gifcase $OUTDIR/giffilelist`
            if [ $priority = 1 ];
            then
               scp $scpfile $OUTDIR/extanl_convert.sh sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/high/$id
               ssh -l sdm ncointra "sh /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/high/$id/extanl_convert.sh $PDY high $id"
            else
               scp $scpfile $OUTDIR/extanl_convert.sh sdm@ncointra:/home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/low/$id
               ssh -l sdm ncointra "sh /home/nco/sdm/sdm/fcstdiv/main/extremes/ext_anl.$PDY/low/$id/extanl_convert.sh $PDY low $id"
            fi
            
         fi
      done
done
