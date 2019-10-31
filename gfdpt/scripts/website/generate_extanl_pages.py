#!/usr/bin/python
from collections import namedtuple
import os.path
import os
from datetime import timedelta, date, datetime

#todaystr = spdy=os.environ['spdy'] + '00'
todaystr = spdy=os.environ['spdycyc'] 

GEPoint=namedtuple("gepoint","rank var lat lon amp")


datadir=os.environ['WEBSTAGEDIR'] + '/extdiff/' + todaystr + '/'

gefile=datadir+'gemout.' + todaystr  

gepoints=[]
numgepoints=0

with open(gefile,'r') as f:
        for line in f:
                columns=line.split()
                p=GEPoint(columns[0],columns[1],columns[2],columns[3],columns[4])
                gepoints.append(p)

for gepoint in gepoints:

    rawvar = gepoint.var[0]
    pheight = gepoint.var[1:]
    if rawvar == 'T' : 
        var = 'tmp'
        variable = 'Temperature'
    elif rawvar == 'Z' : 
        var = 'hgt'
        variable = 'Height'
    elif rawvar == 'W' : 
#        var = 'wnd'
        var = 'hgt'
        variable = 'Wind'
    else: print 'skronk'

    lon = gepoint.lon
    lat = gepoint.lat
    rank = gepoint.rank
    filesuffix =  todaystr + '_' + pheight + '_' + var + '_' + rank + '_' + lat + '_' + lon + '.gif'
    alttextsuffix = ' for ' + variable + ' at ' + pheight + ' hPa Centered at Latitude ' + lat + ', Longitude ' + lon 

    str1 = '<tr><th ALIGN="CENTER">' + rawvar + '(' + lat + ',' + lon + ')</a></th>'
    str2 = '<td ALIGN="CENTER"><a name="' + rank + '"  href="./extdiff/' + todaystr + '/gfs_guess_' + filesuffix + '"><img src="./extdiff/' + todaystr + '/thumbnail.gfs_guess_' + filesuffix + '" alt="GFS Guess ' + alttextsuffix + '"></a></td>'
    str3 = '<td ALIGN="CENTER"><a href="./extdiff/' + todaystr + '/gfs_anl_' + filesuffix + '"><img src="./extdiff/' + todaystr + '/thumbnail.gfs_anl_' + filesuffix + '" alt="GFS Analysis ' + alttextsuffix + '"></a></td>'
    str4 = '<td ALIGN="CENTER"><a href="./extdiff/' + todaystr + '/ecmwf_anl_' + filesuffix + '"><img src="./extdiff/' + todaystr + '/thumbnail.ecmwf_anl_' + filesuffix + '" alt="ECMWF Analysis ' + alttextsuffix + '"></a></td>'
    str5 = '<td ALIGN="CENTER"><a href="./extdiff/' + todaystr + '/ecmwf_anl-gfs_anl_' + filesuffix + '"><img src="./extdiff/' + todaystr + '/thumbnail.ecmwf_anl-gfs_anl_' + filesuffix + '" alt="Difference between ECMWF Analysis and GFS Analysis ' + alttextsuffix + '"></a></td>'
    str6 = '<td ALIGN="CENTER"><a href="./extdiff/' + todaystr + '/gfs_anl-gfs_guess_' + filesuffix + '"><img src="./extdiff/' + todaystr + '/thumbnail.gfs_anl-gfs_guess_' + filesuffix + '" alt="Difference between GFS Analysis and GFS Guess ' + alttextsuffix + '"></a></td>'
    str7 = '</tr>'

    print str1 
    print str2 
    print str3 
    print str4 
    print str5 
    print str6 
    print str7 


print '</table>'
print '<p ALIGN="CENTER"><a href="./extdiff">ARCHIVE OF EXTREME ANALYSIS DIFFERENCES</a></p>'
print '</div>'
print '</div>'
