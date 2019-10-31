#!/opt/ShellB3/2014Q1/bin/python
import read_diag
import matplotlib
matplotlib.use('agg')
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import numpy as np
import sys

def plotglobal(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename,label='brightness temperature (K)'):
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
#    cbar.set_label('brightness temperature (K)')
    cbar.set_label(label)

    plt.title(titlestr,y=1.06)


#plt.savefig('foo.png')
#plt.savefig('foo.pdf')
#plt.savefig('foo.eps')
#plt.savefig('foo.tiff')
    print 'saving ' + outfilename
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

def plotglobalqc(m,diagrad,ichan,titlestr,targetvars,targetamps,targetxpt,targetypt,outfilename):
#    rnbw = cm = plt.get_cmap('rainbow') 
    m.drawcoastlines()
    m.drawmeridians(np.arange(-180.,181.,60.),labels=[True,True,True])
    m.drawparallels(np.arange(-90.,91.,30.),labels=[True,True,True]) 

#    print 'vmin, vmax: ' , vmin,vmax
#    cNorm  = colors.Normalize(vmin=vmin, vmax=vmax)
#    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=rnbw)
#    scalarMap.set_array(obs)

#    colorVal = scalarMap.to_rgba(obs)
    usedqcflags=set(diagrad.qcmark)
#    plottedqcflags=set(3,4,7,8,50,51,53)
#    plottedqcflags=set([3,4,7,8,50,51,53,0,-7])
    plottedqcflags=set([])
    missedqcflags=usedqcflags-plottedqcflags
    if len(missedqcflags):
        print '***** CHANNEL ',ichan,' FLAGS NOT PLOTTED: ',missedqcflags

    for flag in usedqcflags:
        thing=np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==flag ))
        print len(diagrad.channel[thing]), ' points with flag ',flag



    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==3 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='cyan',marker='.',s=15,edgecolors='black',linewidth=0.15,label='gross') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==4 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='green',marker='.',s=15,edgecolors='black',linewidth=0.15,label='interchannel') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==7 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='blue',marker='.',s=15,edgecolors='black',linewidth=0.15,label='cloud') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==8 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='magenta',marker='.',s=15,edgecolors='black',linewidth=0.15,label='temp est') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark == 50 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='purple',marker='.',s=15,edgecolors='black',linewidth=0.15,label='50') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark == 51 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='teal',marker='.',s=15,edgecolors='black',linewidth=0.15,label='51') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark == 53 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='yellow',marker='.',s=15,edgecolors='black',linewidth=0.15,label='53') 

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

#    cbar = m.colorbar(scalarMap,location='bottom',pad="5%")
#    cbar.set_label('brightness temperature (K)')

    plt.title(titlestr,y=1.06)

    plt.legend(bbox_to_anchor=(0., -0.15, 1., .05), loc=3,ncol=4, mode="expand", borderaxespad=0.)
    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
#    sys.exit()
################# END PLOTGLOBALQC ###################################

#def plotglobalqc(m,diagrad,ichan,titlestr,targetvars,targetamps,targetxpt,targetypt,outfilename):
def plottargetqc(m,diagrad,ichan,titlestr,\
        targetvar,targetamp,targetlon,targetlat,outfilename):


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

#    cNorm  = colors.Normalize(vmin=vmin, vmax=vmax)
#    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=rnbw)

#    scalarMap.set_array(obs)
#    colorVal = scalarMap.to_rgba(obs)


    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==3 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='cyan',marker='.',s=300,edgecolors='black',linewidth=0.15,label='gross') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==4 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='green',marker='.',s=300,edgecolors='black',linewidth=0.15,label='interchannel') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==7 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='blue',marker='.',s=300,edgecolors='black',linewidth=0.15,label='cloud') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark ==8 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='magenta',marker='.',s=300,edgecolors='black',linewidth=0.15,label='temp est') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark == 50 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='purple',marker='.',s=300,edgecolors='black',linewidth=0.15,label='50') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark == 51 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='teal',marker='.',s=300,edgecolors='black',linewidth=0.15,label='51') 

    idxqc = np.logical_and((diagrad.channel == ichan ),(diagrad.qcmark == 53 ))
    obslatsqc=diagrad.lat[idxqc]
    obslonsqc=diagrad.lon[idxqc]
    for i,val in enumerate(obslonsqc):
        if val > 180.0  : obslonsqc[i] = val - 360.0
    obsxptqc,obsyptqc=m(obslonsqc,obslatsqc)
    m.scatter(obsxptqc,obsyptqc,c='yellow',marker='.',s=300,edgecolors='black',linewidth=0.15,label='53') 


#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=colorVal,marker='.',s=300,edgecolors='black',linewidth=0.15) 

    targetxpt,targetypt=m(targetlon,targetlat)
    m.scatter(targetxpt,targetypt,marker='+',s=50,c='red',linewidth=1.5) 


#    if obs.size > 0 :
#    cbar = m.colorbar(foo,location='bottom',pad="5%")
#    cbar = m.colorbar(scalarMap,location='bottom',pad="5%")
#    cbar.set_label('brightness temperature (K)')

    plt.title(titlestr,y=1.06)

    plt.legend(bbox_to_anchor=(0., -0.15, 1., .05), loc=3,ncol=4, mode="expand", borderaxespad=0.)

    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
#    plt.close()
################# END PLOTTARGETQC ###################################


