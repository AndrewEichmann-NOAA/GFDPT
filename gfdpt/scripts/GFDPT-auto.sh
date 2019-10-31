#!/bin/sh

cd /global/save/Andrew.Eichmann/r2o/fcstdiv/scripts
sh GFDPT-step-1.sh
sleep 30m
sh GFDPT-step-2.sh
sh GFDPT-step-3.sh
sh GFDPT-step-4.sh
sh GFDPT-step-5.sh
sh GFDPT-step-7.sh
sh GFDPT-step-6.sh
