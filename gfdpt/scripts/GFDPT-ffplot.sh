#!/bin/sh
set -x

source ~/.bashrc

export FFCORRSCRIPTDIR=$GFDPTSCRIPTDIR/ffcorr

#/gpfs/hps3/emc/global/noscrub/Andrew.Eichmann/python/miniconda3/bin/python $FFCORRSCRIPTDIR/plotffstats.py
/usrx/local/prod/python/2.7.13/bin/python $FFCORRSCRIPTDIR/plotffstats.py



