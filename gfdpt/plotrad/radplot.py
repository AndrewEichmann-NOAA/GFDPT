#!/opt/ShellB3/2014Q1/bin/python
import read_diag
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import numpy as np
import sys

def plotglobal(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename):
    rnbw = cm = plt.get_cmap('rainbow') 
    m.drawcoastlines()
    m.drawmeridians(np.arange(-180.,181.,60.),labels=[True,True,True])
    m.drawparallels(np.arange(-90.,91.,30.),labels=[True,True,True]) 

    print 'vmin, vmax: ' , vmin,vmax
    cNorm  = colors.Normalize(vmin=vmin, vmax=vmax)
    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=rnbw)
    scalarMap.set_array(obs)

    colorVal = scalarMap.to_rgba(obs)

    foo=m.scatter(obsxpt,obsypt,c=colorVal,marker='.',s=15,edgecolors='black',linewidth=0.15) 
    m.scatter(targetxpt,targetypt,marker='+',s=50,c='red',linewidth=1.5) 

    textypts=[]
    textxpts=[]
#    for target in range(numgepoints):
    for target,var in enumerate(targetvars):
        xoffset=100000
	yoffset=100000
	targetstr=targetvars[target]+'/'+str(targetamps[target])
	textxpt=targetxpt[target]+xoffset
	textypt=targetypt[target]+yoffset
	textxpts.append(textxpt)
	textypts.append(textypt)
        
	plt.text(textxpt,textypt,targetstr)

#    cbar = m.colorbar(foo,location='bottom',pad="5%")
#    cbar = m.colorbar(fig=plt.gcf(),location='bottom',pad="5%")
#    cbar = m.colorbar(mappable=plt.gci(),location='bottom',pad="5%")
    cbar = m.colorbar(scalarMap,location='bottom',pad="5%")
    cbar.set_label('brightness temperature (K)')

    plt.title(titlestr,y=1.06)


#plt.savefig('foo.png')
#plt.savefig('foo.pdf')
#plt.savefig('foo.eps')
#plt.savefig('foo.tiff')
    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
#    sys.exit()
################# END PLOTGLOBAL ###################################

def plottarget(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,\
        targetvar,targetamp,targetlon,targetlat,outfilename):

#    m = Basemap(projection='stere',width=4800000,height=3600000,\
#                                            resolution='c',\
#                                            lon_0=targetlon,lat_0=targetlat)

    rnbw = cm = plt.get_cmap('rainbow') 
    maxlat=90.0
    minlat=-90.0
    maxlon=180.0
    minlon=-180.0

    maplats=np.arange(minlat,maxlat)
    maplons=np.arange(minlon,maxlon)
  
    parallellats=maplats[maplats%5==0]
    if abs(targetlat) > 50 :
        meridianlons=maplons[maplons%10==0]
    else:
        meridianlons=maplons[maplons%5==0]


    m.drawcoastlines()

    m.drawmeridians(meridianlons,labels=[True,True,True,True])
    m.drawparallels(parallellats,labels=[True,True,True,True]) 

    cNorm  = colors.Normalize(vmin=vmin, vmax=vmax)
    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=rnbw)

    scalarMap.set_array(obs)
    colorVal = scalarMap.to_rgba(obs)

#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
    foo=m.scatter(obsxpt,obsypt,c=colorVal,marker='.',s=300,edgecolors='black',linewidth=0.15) 

    targetxpt,targetypt=m(targetlon,targetlat)
    m.scatter(targetxpt,targetypt,marker='+',s=50,c='red',linewidth=1.5) 


#    if obs.size > 0 :
#    cbar = m.colorbar(foo,location='bottom',pad="5%")
    cbar = m.colorbar(scalarMap,location='bottom',pad="5%")
    cbar.set_label('brightness temperature (K)')

    plt.title(titlestr,y=1.06)


    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
#    plt.close()
################# END PLOTTARGET ###################################
