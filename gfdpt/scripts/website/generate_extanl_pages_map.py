#######!/global/save/Andrew.Eichmann/python/install/anaconda2/bin/python
import sys
from collections import namedtuple
import os
#import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg') # to allow generation of images without x
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
import numpy as np
# get the gribxtremes points

GEPoint=namedtuple("gepoint","rank var lat lon amp")


if len(sys.argv) > 1:
        date = sys.argv[1]



#gefile = '/data/users/aeichmann/plotrad4/gemout.'+date
gefile = 'gemout.'+date

gepointmapfile = 'extdiff/' + date + '/globalmap' + date + '.png'
#gepointmapfile=os.environ['WEBSTAGEDIR'] + '/extdiff/' + date + '/globalmap' + date + '.png'

# the following are the pixels for the origin and kiddie corners of
# the global plots generated, determined empircally since the 
# transforms in matplotlib seem to be buggy.  An 800x600 jpg or 
# png image is assumed.
#originx=111 # 90N,180W
#originy=60
#farx=707 # 90S,180E
#fary=496
originx=98 # 90N,180W
originy=72
farx=718 # 90S,180E
fary=526
xrange=farx-originx
yrange=fary-originy
clicksize=5 # size of clickable area


gepoints=[]
xpts=[]
ypts=[]

with open(gefile,'r') as f:
        for line in f:
                columns=line.split()
                p=GEPoint(columns[0],columns[1],columns[2],columns[3],columns[4],)
                gepoints.append(p)

m = Basemap(projection='mill',llcrnrlat=-90,urcrnrlat=90,\
       llcrnrlon=-180,urcrnrlon=180)


# get corners in projection for use later
maxxpt,maxypt=m(180,90)


# shift lons to -180:180
for gepoint in gepoints:
     if int(gepoint.lon)   > 180  : gepoint.lon = str( int(gepoint.lon) - 360 )

# get targets coords in projection
#lats = [ float(gepoint.lat) for gepoint in gepoints ]
#lons = [ float(gepoint.lon) for gepoint in gepoints ]




m.drawcoastlines()
m.drawmeridians(np.arange(-180.,181.,60.),labels=[True,True,True])
m.drawparallels(np.arange(-90.,91.,30.),labels=[True,True,True])
#m.fillcontinents(color='#cc9966',lake_color='#99ffff')
#m.fillcontinents(color='white',lake_color='#99ffff')
#m.drawmapboundary(fill_color='#99ffff')

for gepoint in gepoints:
    xoffset=100000
    yoffset=100000
    string=gepoint.var+'/'+gepoint.amp
    xpt,ypt=m( float(gepoint.lon) , float(gepoint.lat) )
    xpts.append( xpt )
    ypts.append( ypt )
    m.scatter(xpt,ypt,marker='+',s=50,c='red',linewidth=1.5)
    textxpt=xpt+xoffset
    textypt=ypt+yoffset
    plt.text(textxpt,textypt,string)
    plt.title('gribextremes points, ' + date, y=1.06)

#plt.show()
plt.savefig('globalmap' + date + '.png')

#sys.exit()


#print('<div class="tabBottomContent">')
#print(' <div class="aligncenter">')
#print(' <p><b>CURRENT EXTREME ANALYSIS CASES</b></p>')

imgmapline='<img src="' + gepointmapfile + '" width="800" height="600" usemap="#' + date + '">\n'
print(imgmapline)

print('</div>')

print('<map name="' + date + '">\n')

for gepoint in gepoints:

    idx = int( gepoint.rank ) - 1

    ycoord=str(int(((maxypt-ypts[idx])/maxypt)*yrange)+originy)
    xcoord=str(int((xpts[idx]/maxxpt)*xrange)+originx)

    latlonstr=gepoint.lat + ',' + gepoint.lon 

    pre='  <area shape="circle" coords="'
#    print( pre + xcoord + ',' + ycoord + ',' + str(clicksize) + '" href="#' + gepoint.rank + '" >\n' )
    print( pre + xcoord + ',' + ycoord + ',' + str(clicksize) + '" href="#' + gepoint.rank + '" >' )

#print('</map>\n')
print('</map>')


