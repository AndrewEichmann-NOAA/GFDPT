import read_diag
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import numpy as np
import sys


def plotglobal(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename):
    m.drawcoastlines()
    m.drawmeridians(np.arange(-180.,181.,60.),labels=[True,True,True])
    m.drawparallels(np.arange(-90.,91.,30.),labels=[True,True,True]) 

    print vmin,vmax
    cNorm  = colors.Normalize(vmin=vmin, vmax=vmax)
    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=rnbw)
    scalarMap.set_array(obs)

    colorVal = scalarMap.to_rgba(obs)

#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=50,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=40,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=30,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=20,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=10,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=15,edgecolors='black',linewidth=0.15) 
    foo=m.scatter(obsxpt,obsypt,c=colorVal,marker='.',s=15,edgecolors='black',linewidth=0.15) 
    m.scatter(targetxpt,targetypt,marker='+',s=50,c='red',linewidth=1.5) 


    textypts=[]
    textxpts=[]

    for target in range(numgepoints):
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

#    plt.title('Tb ' + date + ' ' + instrument + ' Ch ' + str(ichan),y=1.06)
    plt.title(titlestr,y=1.06)


#plt.savefig('foo.png')
#plt.savefig('foo.pdf')
#plt.savefig('foo.eps')
#plt.savefig('foo.tiff')
    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
################# END PLOTGLOBAL ###################################

def plottarget(obs,obslats,obslons,maxlat,minlat,maxlon,minlon,titlestr,vmin,vmax,\
        targetvar,targetamp,targetlon,targetlat,outfilename):

#    m = Basemap(projection='mill',llcrnrlat=minlat,urcrnrlat=maxlat,\
#		                            llcrnrlon=minlon,urcrnrlon=maxlon)
#    m = Basemap(projection='stere',llcrnrlat=minlat,urcrnrlat=maxlat,\
#		                            llcrnrlon=minlon,urcrnrlon=maxlon, \
#                                            lon_0=targetlon,lat_0=targetlat)
    m = Basemap(projection='stere',width=4800000,height=3600000,\
                                            resolution='c',\
                                            lon_0=targetlon,lat_0=targetlat)

    maxlat=90.0
    minlat=-90.0
    maxlon=180.0
    minlon=-180.0

    maplats=np.arange(minlat,maxlat)
    maplons=np.arange(minlon,maxlon)
  
#    print maplats,maplons


    parallellats=maplats[maplats%5==0]
    if abs(targetlat) > 50 :
        meridianlons=maplons[maplons%10==0]
    else:
        meridianlons=maplons[maplons%5==0]

#    print parallellats,meridianlons



    m.drawcoastlines()

    m.drawmeridians(meridianlons,labels=[True,True,True,True])
    m.drawparallels(parallellats,labels=[True,True,True,True]) 

    print targetlat, targetlon, vmin,vmax    
#    if obs.size > 0 :
#    cNorm  = colors.Normalize(vmin=min(obs), vmax=max(obs))

    cNorm  = colors.Normalize(vmin=vmin, vmax=vmax)
    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=rnbw)

    scalarMap.set_array(obs)
    colorVal = scalarMap.to_rgba(obs)

    obsxpt,obsypt=m(obslons,obslats)

#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
#    foo=m.scatter(obsxpt,obsypt,c=obs,cmap=cm,marker='.',s=300,edgecolors='black',linewidth=0.15) 
    foo=m.scatter(obsxpt,obsypt,c=colorVal,marker='.',s=300,edgecolors='black',linewidth=0.15) 

    print foo.get_clim()

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


date='2015092200'
date='2015092600'
#date='2015101700'
#date='2015101800'
#date='2015121000'


platform='npp'
instrument='atms'
instrument='atms_npp'
BorA='anl'
#BorA='ges'

#makeplots=True
makeplots=False

# get the gribxtremes points

targetvars=[]
targetlats=[]
targetlons=[]
targetamps=[]
numgepoints=0

gefile = '/data/users/aeichmann/gfdpt-checkout/gfdpt/gribextremes/run'+date+'/fort.55'
with open(gefile,'r') as f:
	for line in f:
		columns=line.split()
		targetvars.append(columns[1])
		targetlats.append(int(columns[2]))
		targetlons.append(int(columns[3]))
		targetamps.append(float(columns[4]))
		numgepoints=numgepoints+1


print targetvars
print targetlats
print targetlons
print targetamps

#sys.exit()




diagfiledir='/data/users/aeichmann/COATsvn/radstat/'
diagfile = diagfiledir + 'diag_'+instrument+'_'+BorA+'.'+date
#diagfile ='/data/users/aeichmann/COATsvn/radstat/diag_atms_npp_anl.' + date
diag_rad = read_diag.diag_rad(diagfile,endian='big')


print 'total number of obs = ',diag_rad.nobs
diag_rad.read_obs()
# print o-f stats for one channel

channels= list(set(diag_rad.channel))
#channels=[1,2,3]



# llcrnrlat,llcrnrlon,urcrnrlat,urcrnrlon
# are the lat/lon values of the lower left and upper right corners
# of the map.
# resolution = 'c' means use crude resolution coastlines.

#    m = Basemap(projection='mill',llcrnrlat=-90,urcrnrlat=90,\
#		                            llcrnrlon=-180,urcrnrlon=180,resolution='c')
m = Basemap(projection='mill',llcrnrlat=-90,urcrnrlat=90,\
		                            llcrnrlon=-180,urcrnrlon=180)

rnbw = cm = plt.get_cmap('rainbow') 

for i,val in enumerate(targetlons):
     if val > 180.0  : targetlons[i] = val - 360.0

targetxpt,targetypt=m(targetlons,targetlats)


tablelines=[[] for x in xrange(numgepoints)]

for ichan in channels:
#for ichan in [6]:

    # get indices of all matching channels
    idxall = diag_rad.channel == ichan
    # how many obs total
    nobsall = idxall.sum()
    # get indices of used obs with matching channels
    idx = np.logical_and(np.logical_and(diag_rad.channel == ichan, diag_rad.used == 1), diag_rad.oberr < 1.e9)
    nobs = idx.sum()
    # skip if no obs used
    if nobs == 0: continue 
    fitsq = ((diag_rad.hx[idx]-diag_rad.obs[idx])**2).mean()

    print diag_rad.obs[6],diag_rad.hx[6],diag_rad.biascorr[6],diag_rad.biaspred[1:,6].sum()
    print nobs,'obs used for channel',ichan,'out of',nobsall,'rms o-f', np.sqrt(fitsq)

    obslats=diag_rad.lat[idx]
    obslons=diag_rad.lon[idx]
    for i,val in enumerate(obslons):
        if val > 180.0  : obslons[i] = val - 360.0
    obsxpt,obsypt=m(obslons,obslats)

    
    obs=diag_rad.obs[idx]
    if BorA== 'anl' :
    	titlestr = 'Tb ana ' + date + ' ' + instrument + ' Ch ' + str(ichan)
	outfilename=date+'/'+instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + '.jpg'
    else:
    	titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan)
	outfilename=date+'/'+instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + '.jpg'
    vmin=min(obs)
    vmax=max(obs)
    if makeplots:
    	print 'generating ' + outfilename 
	plotglobal(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)

    omb_bc = diag_rad.obs[idx] - diag_rad.hx[idx]
    if BorA== 'anl' :
	titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    	outfilename=date+'/'+instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + '.jpg'
    else:
	titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    	outfilename=date+'/'+instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + '.jpg'

    vmin=min(omb_bc)
    vmax=max(omb_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	plotglobal(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)

#    hx = diag_rad.hx[idx]
#    titlestr = 'hx (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
#    outfilename=date+'/'+instrument+'_hx_Ch%02d' % ichan + '_' + date + '.jpg'
#    plotglobal(m,hx,obsxpt,obsypt,titlestr,targetvars,targetamps,targetxpt,targetypt,outfilename)

    for i,val in enumerate(targetvars):

   		
        targetlat=targetlats[i]
        targetlon=targetlons[i]

        radius=15

        maxlat=targetlat+radius
        minlat=targetlat-radius

        if abs(targetlat) > 50: radius = radius * 2
 
        maxlon=targetlon+radius
        minlon=targetlon-radius


        if maxlat > 90.0: maxlat = 90.0
        if minlat < -90.0: minlat = -90.0

        newlons=diag_rad.lon[idx]
        newlats=diag_rad.lat[idx]
        newobs=diag_rad.obs[idx]

        for j,lontmp in enumerate(newlons):
            if lontmp > 180.0  : newlons[j] = lontmp - 360.0

#        targetidx = np.logical_and(minlat <= diag_rad.lats[idx] <= maxlat, minlon <= diag_rad.lons[idx] <= maxlon )
        targetidx = np.logical_and(  \
                np.logical_and( newlats >= minlat , newlats <= maxlat ), \
                np.logical_and( newlons >= minlon , newlons <= maxlon ))

#        obs=newobs[targetidx]
#        obslats=newlats[targetidx]
#        obslons=newlons[targetidx]
        obs=newobs
        obslats=newlats
        obslons=newlons
        if (obs.size > 0) and (abs(targetlat) < 80): # targetlat for workaround basemap bug
            if BorA== 'anl' :
        	titlestr = 'Tb ana ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val
        	outfilename=date+'/'+instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + '_' + \
                        val + '_' + str(targetlat) + '_' + str(targetlon) + '.jpg'
            else:
        	titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val
        	outfilename=date+'/'+instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + '_' + \
                        val + '_' + str(targetlat) + '_' + str(targetlon) + '.jpg'

            vmin=min(obs)
	    vmax=max(obs)

            if makeplots:
                print 'generating ' + outfilename 
                plottarget(obs,obslats,obslons,maxlat,minlat,maxlon,minlon,titlestr,vmin,vmax,targetvars[i], \
                        targetamps[i],targetlon,targetlat,outfilename)

#	    omb_bc_tgt = omb_bc[targetidx]
	    omb_bc_tgt = omb_bc

            if BorA== 'anl' :
	        titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val
                outfilenamebase=date+'/'+instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + '_' + \
                         val + '_' + str(targetlat) + '_' + str(targetlon) 
            else:
	        titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val
        	outfilenamebase=date+'/'+instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + \
                        '_' + val + '_' + str(targetlat) + '_' + str(targetlon) 

            tableline = '<tr><td><img src="'

	    vmin=-1.0
	    vmax=1.0
	    outfilename = outfilenamebase + '_fix.jpg'
            tableline = tableline + outfilename
            if makeplots:
                print 'generating ' + outfilename 
                plottarget(omb_bc_tgt,obslats,obslons,maxlat,minlat,maxlon,minlon,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

            tableline = tableline + '"></td><td><img src="'

            vmin=min(omb_bc_tgt)
	    vmax=max(omb_bc_tgt)
#	    outfilename = outfilenamebase + '_rel.tiff'
	    outfilename = outfilenamebase + '_rel.jpg'
            tableline = tableline + outfilename
            if makeplots:
                print 'generating ' + outfilename 
                plottarget(omb_bc_tgt,obslats,obslons,maxlat,minlat,maxlon,minlon,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

            tableline = tableline +  '"></td></tr>\n'

            tablelines[i].append(tableline)

#        else: tablelines[i].append( -1 )




for i,val in enumerate(targetvars):

    targetlat=targetlats[i]
    targetlon=targetlons[i]

 
    if BorA== 'anl' :
        outfilename=instrument+'_ana_OmA_' + date + '_' + \
                        str(targetlat) + '_' + str(targetlon) + '.html'
    else:
      	outfilename=instrument+ '_ges_OmB_' + date + \
                        '_' + val + '_' + str(targetlat) + '_' + str(targetlon) + '.html'


#   gepointpagename='grid_' + val + '_' + str(targetlat) + '_' + str(targetlon) + '.html'
    with open(outfilename, "w") as f:
        print 'writing ' + outfilename
        header ='<html><head><title>Sample CGI Script</title></head><body>\n'
        f.write(header)
        f.write('<table border="1">\n')
#        for ichan in channels:
        for j in  range(len(tablelines[i])):
#        for ichan in [0]:
            if tablelines[i][j] :
                f.write(tablelines[i][j])   
        f.write('</table></body></html>\n')


