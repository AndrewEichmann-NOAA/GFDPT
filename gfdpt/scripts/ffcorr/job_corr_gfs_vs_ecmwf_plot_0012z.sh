# Krishna Kumar NESDIS/JCSDA March 20, 2017
set -x
#. /u/$LOGNAME/.profile
date
export envir=prod
#
export grp=global
#
#
# First run the 12z cycle for the previous day since the ECMWF GRIB1 files
# reach NCEP after a delay of 24 hours 
#
export cyc=12
export NET=gfs
export RUNV=ecmwf
export job=corr_${NET}_vs_${RUNV}_${cyc}
#export runtmp=/stmpp1/$LOGNAME/fcstdiv/plot$cyc
export runtmp=/gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/plot$cyc
#
export homedir=/$grp/save/$LOGNAME/GFDPT
#
mkdir -p $runtmp
export output=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
export error=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
#
#sh $homedir/scripts/ffcorr/vsdbjob_submit2.sh 
sh $FFCORRSCRIPTDIR/vsdbjob_submit2.sh 
#
# Next run the same scripts for 00z cycle for the current day since there is
# not a considerable delay in receiving the ECMWF GRIB1 files
#
export cyc=00
export NET=gfs
export RUNV=ecmwf
export job=corr_${NET}_vs_${RUNV}_${cyc}
#export runtmp=/stmpp1/$LOGNAME/fcstdiv/plot$cyc
export runtmp=/gpfs/hps2/stmp/Andrew.Eichmann/fcstdiv/plot$cyc
#
export homedir=/$grp/save/$LOGNAME/GFDPT
#
mkdir -p $runtmp
export output=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}
export error=$runtmp/corr_${NET}_vs_${RUNV}_${cyc}

sh $FFCORRSCRIPTDIR/vsdbjob_submit2.sh
#
# Run the e-mail script
#
###sh $homedir/scripts/vsdb_f-f_dalert.sh

exit
