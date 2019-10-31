#!/bin/sh
set -x

export FFCORRSCRIPTDIR=$GFDPTSCRIPTDIR/ffcorr
export tmpdir=


cd $GFDPTTMPDIR

sh $FFCORRSCRIPTDIR/job_corr_gfs_vs_ecmwf_0012z.sh
#sh $FFCORRSCRIPTDIR/job_corr_gfs_vs_ecmwf.sh
sh $FFCORRSCRIPTDIR/job_corr_gfs_vs_ecmwf_plot_0012z.sh
#exit

export jobids=`cat $GFDPTTMPDIR/jobids.$spdy | tr '\n' ' '`

numjobs="1"
while [ $numjobs -gt 0 ]
do 
  sleep 1m
  bjobs
  numjobs=`bjobs  -noheader $jobids | grep 'PEND\|RUN' | wc -l`
done

#exit



export cyc=00
sh $FFCORRSCRIPTDIR/vsdbjob_retrieve_textfiles.sh
sh $FFCORRSCRIPTDIR/changetextfiles.sh

export cyc=12
sh $FFCORRSCRIPTDIR/vsdbjob_retrieve_textfiles.sh
sh $FFCORRSCRIPTDIR/changetextfiles.sh

rm $FFCORRDIR/timeseries/*
python $FFCORRSCRIPTDIR/append_textfiles_for_plotting.py



