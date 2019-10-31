#!/usrx/local/python/2.7.12/bin/python
import sys
import os
import subprocess
from datetime import timedelta, date, datetime
import time


spdycyc = os.environ['cyc']
spdy = os.environ['spdy'] + spdycyc
datasources = os.environ['DATASOURCES']
tarfiledir = os.environ['TARFILEDIR']

daysback = 0

if len(sys.argv) > 1:
    daysback = int(sys.argv[1])

print spdy
spdyyear=int(spdy[0:4])
spdymonth=int(spdy[4:6])
spdyday=int(spdy[6:8])
spdycyc=int(spdy[8:10])

print spdy,spdyyear,spdymonth,spdyday,spdycyc


startdatetime=datetime(spdyyear,spdymonth,spdyday,spdycyc)
displaydate=startdatetime.strftime("%Y%m%d%H")

for cdatetime in (startdatetime - timedelta(n) for n in range(daysback,-1,-1)):

    print cdatetime
    cdate=cdatetime.strftime("%Y%m%d")
    ccyc=cdatetime.strftime("%H")
    tarname = tarfiledir + '/gdas.' + cdate + '/' + ccyc + '/gdas.t' + ccyc + 'z.radstat'
#    tarname = tarfiledir + '/gdas.' + cdate + '/gdas.t' + ccyc + 'z.radstat'
#    tarname = tarfiledir + '/' + cdate + '/gdas.t' + ccyc + 'z.radstat'
    print tarname
   
    for datasource in datasources.split():
        
        print datasource
        diagfilename = 'diag_' + datasource + '_anl.' + cdate + ccyc 
        if not os.path.isfile(diagfilename):
            subprocess.call(['tar', 'xvf', tarname, diagfilename + '.gz' ])
            subprocess.call(['gunzip', '-f', diagfilename + '.gz' ])

        diagfilename = 'diag_' + datasource + '_ges.' + cdate + ccyc
        if not os.path.isfile(diagfilename):
            subprocess.call(['tar', 'xvf', tarname, diagfilename + '.gz' ])
            subprocess.call(['gunzip', '-f', diagfilename + '.gz' ])


sys.exit()

