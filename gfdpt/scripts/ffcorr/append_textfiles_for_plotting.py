#####!/usr/bin/python

import os
from datetime import timedelta, date, datetime
from subprocess import call

indir=os.environ['FFCORRDIR']
spdy=os.environ['spdy']
outdir=indir+'/timeseries/'

oneday = timedelta( days = 1 )

enddate00 = date( int(spdy[0:4]), int(spdy[4:6]), int(spdy[6:8]) )
enddate12 = enddate00 - oneday
startdate = enddate00 - timedelta(days=80)
variables = ['HGT', 'T', 'U', 'V', 'WIND' ]
pheights = ['250','500','700','850','1000']
regions = [ 'NH' , 'SH' ]
stats = [ 'cor' , 'rms' ]

if not os.path.isdir(outdir): os.mkdir(outdir)
os.chdir(outdir)

for stat in stats:
    for var in variables:
        for pheight in pheights:
            if pheight == '700' and var != 'HGT' : continue
            if pheight == '1000' and var == 'WIND' : continue
            if pheight == '1000' and var == 'U' : continue
            if pheight == '1000' and var == 'V' : continue
            for region in regions:
                filenamebase='mean' + stat + '_' + var + '_P' + pheight + '_G2' + \
                        region + 'X' 

                outfile=outdir + filenamebase + '00Z.txt'
                dt = startdate
                print( 'writing ' + outfile)
                with open(outfile, "a") as ofile:
                    while dt <= enddate00:
                        infile = filenamebase + '.txt'
                        datestr=dt.strftime("%Y%m%d") + '00'
                        infilepath=indir + '/' + datestr + '/mapfiles/' + infile
                        print ('trying to open ' + infilepath)
                        if os.path.isfile(infilepath):
                            for line in open(infilepath):
                                ofile.write(datestr + '    ' + line)
                        else:
                            print( 'couldnt find ' + infilepath)
                            ofile.write(datestr + '         ')
                            for i in range(13):
                                ofile.write('-99.0     ')
                            ofile.write('\n')
                        dt += oneday

                outfile=outdir + filenamebase + '12Z.txt'
                dt = startdate 
                print( 'writing ' + outfile)
                with open(outfile, "a") as ofile:
                    while dt <= enddate12:
                        infile = filenamebase + '.txt'
                        datestr=dt.strftime("%Y%m%d") + '12'
                        infilepath=indir + '/' + datestr + '/mapfiles/' + infile
                        print( 'trying to open ' + infilepath)
                        if os.path.isfile(infilepath):
                            for line in open(infilepath):
                                ofile.write(datestr + '    ' + line)
                        else:
                            print( 'couldnt find ' + infilepath)
                            ofile.write(datestr + '         ')
                            for i in range(13):
                                ofile.write('-99.0     ')
                            ofile.write('\n')
                        dt += oneday




                                
                                


