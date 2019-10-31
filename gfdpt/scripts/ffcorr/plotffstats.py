#####!/usr/bin/python
import numpy as np
import ffplotgen
import matplotlib.pyplot as plt
import sys
#import os.path
import os
from datetime import timedelta, date, datetime
from subprocess import call

todaystr = spdy=os.environ['spdy']
today = datetime( int(spdy[0:4]), int(spdy[4:6]), int(spdy[6:8]), 0 )
indir=os.environ['FFCORRDIR'] + '/timeseries/'
outdir=os.environ['FFCORRDIR'] + '/plots/' + todaystr + '/'


numdays=30
variables=['HGT', 'T', 'U', 'V', 'WIND' ]
pheights=['250','500','700','850','1000']
regions=['NH','SH']
stats=['cor','rms']

ALERTR=False
ALERTY=False

if not os.path.isdir(outdir): os.mkdir(outdir)
os.chdir(outdir)

for stat in stats:
    for var in variables:
       for pheight in pheights:

           if pheight == '700' and var != 'HGT' : continue

           for region in regions:


              field= var + '_P' + pheight + '_G2' + region

              infilename00Z=indir + 'mean' + stat + '_' + field + 'X00Z.txt'
              infilename12Z=indir + 'mean' + stat + '_' + field + 'X12Z.txt'

              if not os.path.isfile(infilename00Z): 
                 print( 'passing on ' + infilename00Z)
                 continue
              if not os.path.isfile(infilename12Z): 
                 print( 'passing on ' + infilename12Z)
                 continue

              filelines00Z=[]
              filelines12Z=[]

              rawicdates00Z=[]
              rawicdates12Z=[]

              ffcor00zd={}
              ffcor12zd={}

              print( 'reading ', infilename00Z)
              with open(infilename00Z,'r') as f:
                  for line in f:
                      filelines00Z.append(line.split())

              for line in filelines00Z:
                 date = line[0]
                 initdate = datetime(int(date[0:4]),int(date[4:6]),int(date[6:8]), \
                             int(date[8:10]))
                 ffcor00zd[initdate] = line
     
              print( 'reading ', infilename12Z)
              with open(infilename12Z,'r') as f:
                  for line in f:
                      filelines12Z.append(line.split())

              for line in filelines12Z:
                 date = line[0]
                 initdate = datetime(int(date[0:4]),int(date[4:6]),int(date[6:8]), \
                             int(date[8:10]))
                 ffcor12zd[initdate] = line
 

              for fcstday in range(1,11):
                 startidate = today - timedelta( days = numdays + fcstday )
                 ffcor00Z=[]
                 ffcor12Z=[]
                 vdates00Z=[]
                 vdates12Z=[]
                 std00Z=[]
                 std12Z=[]
                 mean00Z=[]
                 mean12Z=[]

                 for idate in ( startidate + timedelta( days = n ) \
                             for n in range( numdays + fcstday + 1 ) ):

                    line = ffcor00zd[ idate ] 
                    ffcor00Z.append(float(line[fcstday+3]))
                    vdate00Z = idate + timedelta( days = fcstday  )                             
                    vdates00Z.append( vdate00Z )

                    idate12Z = idate - timedelta( hours = 12 )
                    line = ffcor12zd[ idate12Z ] 
                    ffcor12Z.append(float(line[fcstday+3]))
                    vdate12Z = idate12Z + timedelta( days = fcstday  )                             
                    vdates12Z.append( vdate12Z )

                    stats00Z=[]
                    stats12Z=[]

                    startsdate = idate - timedelta( days = numdays ) 
                    for sdate in ( startsdate + timedelta( days = m ) \
                                for m in range( numdays ) ):
                       line =  ffcor00zd[ sdate ]
                       stats00Z.append(float(line[fcstday+3]))
                       sdate12Z = sdate - timedelta( hours = 12 )
                       line =  ffcor12zd[ sdate12Z ]
                       stats12Z.append(float(line[fcstday+3]))
                 
                    stats00Z=np.array(stats00Z)
                    stats12Z=np.array(stats12Z)

                    # some relation values are -99.0, make them nans
                    stats00Z[np.where(stats00Z < -1.0 )]=np.NAN
                    stats12Z[np.where(stats12Z < -1.0 )]=np.NAN

                    mean00Z.append( stats00Z[np.isfinite(stats00Z)].mean() )
                    std00Z.append( stats00Z[np.isfinite(stats00Z)].std() )
                    mean12Z.append( stats12Z[np.isfinite(stats12Z)].mean() )
                    # nanmean doesn't work, so...
                    std12Z.append( stats12Z[np.isfinite(stats12Z)].std() )

#                 print mean00Z
#                 print std00Z

                 (RED, YELLOW) = ffplotgen.ffplot(ffcor00Z,ffcor12Z,vdates00Z, \
                           var,pheight,region,numdays,fcstday,stat,  \
                           mean00Z,mean12Z,std00Z,std12Z)

                 ALERTR = RED or ALERTR
                 ALERTY = YELLOW or ALERTY

call('pwd')
#call('sh ../notify.sh')
call('/global/save/Andrew.Eichmann/r2o/fcstdiv/scripts//notify.sh')

