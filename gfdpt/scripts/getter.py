#####!/usr/bin/python

import os
from datetime import timedelta, date, datetime
from subprocess import call

#indir=os.environ['FFCORRDIR']
indir='/gpfs/hps3/emc/global/noscrub/Andrew.Eichmann/GFDPT/data/dev/ffcorr'
#spdy=os.environ['spdy']
spdy='20191031'
oneday = timedelta( days = 1 )

filename = 'missingdates'

enddate00 = date( int(spdy[0:4]), int(spdy[4:6]), int(spdy[6:8]) )
enddate12 = enddate00 - oneday
startdate = enddate00 - timedelta(days=10)

if not os.path.isdir(indir): 
   print('slronk')
   sys.exit()
os.chdir(indir)


listdir = os.listdir(indir)
                                
print(listdir)
missing=[]


f = open(filename,'w')
cdatetime = startdate
while cdatetime <= enddate00:
   print(cdatetime)
   cdate = cdatetime.strftime("%Y%m%d%H")
   print cdate
   if cdate not in listdir:
      missing.append(cdate)
   f.write(cdate + '\n')
   cdatetime += oneday
     
f.close()
print(missing)

#for cdatetime in (startdate - timedelta(n-1) for n in range(daycount,0,-1)):

