#!/usr/bin/python
import os.path
import os
from datetime import timedelta, date, datetime

#today=date.today()
#todaystr=today.strftime("%Y%m%d")
todaystr = spdy=os.environ['spdy']
outdir=os.environ['WEBSTAGEDIR'] + '/charts'

plotdir='../ffcorr'

variables=['HGT', 'T', 'U', 'V', 'WIND' ]
pheights=['250','500','700','850','1000']
regions=['NH','SH']
stats=['cor','rms']
fcstdays=range(1,11)

#if not os.path.isdir(outdir): os.mkdir(outdir)
os.chdir(outdir)

for fcstday in fcstdays:
    for stat in stats:
        for var in variables:
           for pheight in pheights:

               if pheight == '700' and var != 'HGT' : continue

               filename='fcst' + stat + '_' + var + '_P' + pheight +  '_day' + \
                     str(fcstday) + '.html'
                                
               with open(filename, "w") as f:
                  for region in regions:

                     imgfilename='fcst' + stat + '_' + var + '_P' + pheight + '_' + region +\
                          '_day' + str(fcstday) + '.png'
                  
                     prefix='<img src="' + plotdir + '/' + todaystr + '/'
                     suffix='">\n'
                  
                     f.write(prefix+imgfilename+suffix)
                      


