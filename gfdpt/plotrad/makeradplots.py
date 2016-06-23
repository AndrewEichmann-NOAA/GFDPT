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

    plt.title(titlestr,y=1.06)


#plt.savefig('foo.png')
#plt.savefig('foo.pdf')
#plt.savefig('foo.eps')
#plt.savefig('foo.tiff')
    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
################# END PLOTGLOBAL ###################################

def plottarget(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,\
        targetvar,targetamp,targetlon,targetlat,outfilename):

#    m = Basemap(projection='stere',width=4800000,height=3600000,\
#                                            resolution='c',\
#                                            lon_0=targetlon,lat_0=targetlat)

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

makeplots=True
#makeplots=False

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
diaganlfile = diagfiledir + 'diag_'+instrument+'_anl.'+date
diagradanl = read_diag.diag_rad(diaganlfile,endian='big')

diaggesfile = diagfiledir + 'diag_'+instrument+'_ges.'+date
diagradges = read_diag.diag_rad(diaggesfile,endian='big')


print 'total number anl obs = ',diagradanl.nobs
diagradanl.read_obs()
# print o-f stats for one channel

print 'total number ges obs = ',diagradges.nobs
diagradges.read_obs()
# print o-f stats for one channel

# assumes channels are same for both anl and ges
channels= list(set(diagradanl.channel))

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

#for ichan in channels:
for ichan in [6,7]:

    # get indices of all matching channels
    idxallanl = diagradanl.channel == ichan
    idxallges = diagradges.channel == ichan
    # how many obs total
    nobsallanl = idxallanl.sum()
    nobsallges = idxallges.sum()
    # get indices of used obs with matching channels

    # oberr varies between anl and ges, so eliminated for the moment to make them match 

#    idxanl = np.logical_and(np.logical_and(diagradanl.channel == ichan, diagradanl.used == 1), diagradanl.oberr < 1.e9)
#    idxanl = np.logical_and(diagradanl.channel == ichan, diagradanl.used == 1)
    idxanl = np.logical_and(np.logical_and(np.logical_and(diagradanl.channel == ichan, \
            diagradanl.used == 1), diagradanl.oberr < 1.e9), diagradges.oberr < 1.e9)

    nobsanl = idxanl.sum()
#    idxges = np.logical_and(np.logical_and(diagradges.channel == ichan, diagradges.used == 1), diagradges.oberr < 1.e9)
#    idxges = np.logical_and(diagradges.channel == ichan, diagradges.used == 1)
    idxges = np.logical_and(np.logical_and(np.logical_and(diagradges.channel == ichan, \
            diagradges.used == 1), diagradges.oberr < 1.e9), diagradanl.oberr < 1.e9)
    nobsges = idxges.sum()

    if nobsges != nobsanl:
        print 'nobsges = ' + str(nobsges) + ', nobsanl = ' + str(nobsanl)
        sys.exit()

    # skip if no obs used
    if nobsanl == 0: continue 
    fitsq = ((diagradanl.hx[idxanl]-diagradanl.obs[idxanl])**2).mean()

    print diagradanl.obs[6],diagradanl.hx[6],diagradanl.biascorr[6],diagradanl.biaspred[1:,6].sum()
    print nobsanl,'anl: obs used for channel',ichan,'out of',nobsallanl,'rms o-f', np.sqrt(fitsq)

    print diagradges.obs[6],diagradges.hx[6],diagradges.biascorr[6],diagradges.biaspred[1:,6].sum()
    print nobsges,'ges: obs used for channel',ichan,'out of',nobsallges,'rms o-f', np.sqrt(fitsq)

    obslats=diagradanl.lat[idxanl]
    obslons=diagradanl.lon[idxanl]

    if any(obslats != diagradges.lat[idxges]) or any(obslons != diagradges.lon[idxges]):
        print 'skronk'
        sys.exit()

    for i,val in enumerate(obslons):
        if val > 180.0  : obslons[i] = val - 360.0

    obsxpt,obsypt=m(obslons,obslats)
    
    field=diagradanl.obs[idxanl]
    titlestr = 'Tb ana ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=date+'/'+instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + '.jpg'
    vmin=min(field)
    vmax=max(field)
    if makeplots:
    	print 'generating ' + outfilename 
	plotglobal(m,field,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)

    field=diagradges.obs[idxges]
    titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=date+'/'+instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + '.jpg'
    vmin=min(field)
    vmax=max(field)
    if makeplots:
    	print 'generating ' + outfilename 
	plotglobal(m,field,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)


    oma_bc = diagradanl.obs[idxanl] - diagradanl.hx[idxanl]
    titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=date+'/'+instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + '.jpg'

    vmin=min(oma_bc)
    vmax=max(oma_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	plotglobal(m,oma_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)


    omb_bc = diagradanl.obs[idxanl] - diagradanl.hx[idxanl]
    titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=date+'/'+instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + '.jpg'
    vmin=min(omb_bc)
    vmax=max(omb_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	plotglobal(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)


    for i,val in enumerate(targetvars):

   		
        targetlat=targetlats[i]
        targetlon=targetlons[i]

        
        if abs(targetlat) > 80: continue # workaround for basemap bug

        m = Basemap(projection='stere',width=4800000,height=3600000,\
                                            resolution='c',\
                                            lon_0=targetlon,lat_0=targetlat)

        obsxpt,obsypt=m(obslons,obslats)

        newobs=diagradanl.obs[idxanl]


        obs=newobs
        if (obs.size > 0) :
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
                plottarget(m,obs,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                        targetamps[i],targetlon,targetlat,outfilename)

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
                plottarget(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

            tableline = tableline + '"></td><td><img src="'

            vmin=min(omb_bc)
	    vmax=max(omb_bc)
#	    outfilename = outfilenamebase + '_rel.tiff'
	    outfilename = outfilenamebase + '_rel.jpg'
            tableline = tableline + outfilename
            if makeplots:
                print 'generating ' + outfilename 
                plottarget(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
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


