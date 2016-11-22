#!/opt/ShellB3/2014Q1/bin/python
import read_diag
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import numpy as np
import sys
import os
import radplot as rp


date='2015092100'
date='2015092200'
date='2015092600'
date='2015101700'
date='2015101800'
date='2015121000'
date='2016071100'


#instrument='atms_npp'
#instrument='amsua_n19'
instrument='amsua_n18'

makeplots=True
makeplots=False

imgtype='.png'

if len(sys.argv) > 1:
    date = sys.argv[1]
    instrument = sys.argv[2]

print date,instrument


if not os.path.isdir(date): os.mkdir(date)
os.chdir(date)

if not os.path.isdir(instrument): os.mkdir(instrument)
os.chdir(instrument)


# get the gribxtremes points

targetvars=[]
targetlats=[]
targetlons=[]
targetamps=[]
targetstrs=[]
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

for i,val in enumerate(targetvars):
    targetstrs.append(val+'_'+str(targetlats[i])+'_'+str(targetlons[i]))

print targetvars
print targetlats
print targetlons
print targetamps
print targetstrs

#sys.exit()

listoftargets=[]


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

maxxpt,maxypt=m(180,90)

rnbw = cm = plt.get_cmap('rainbow') 

for i,val in enumerate(targetlons):
     if val > 180.0  : targetlons[i] = val - 360.0

targetxpt,targetypt=m(targetlons,targetlats)


omatargetimagesfix=[[] for x in xrange(numgepoints)]
ombtargetimagesfix=[[] for x in xrange(numgepoints)]
ambtargetimagesfix=[[] for x in xrange(numgepoints)]
omatargetimagesrel=[[] for x in xrange(numgepoints)]
ombtargetimagesrel=[[] for x in xrange(numgepoints)]
ambtargetimagesrel=[[] for x in xrange(numgepoints)]
anlobstargetimages=[[] for x in xrange(numgepoints)]
gesobstargetimages=[[] for x in xrange(numgepoints)]

#channels=[1,2,3]

for ichan in channels:

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
    outfilename=instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + imgtype
    vmin=min(field)
    vmax=max(field)
    if makeplots:
    	print 'generating ' + outfilename 
	rp.plotglobal(m,field,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)

    field=diagradges.obs[idxges]
    titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + imgtype
    vmin=min(field)
    vmax=max(field)
    if makeplots:
    	print 'generating ' + outfilename 
	rp.plotglobal(m,field,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)


    oma_bc = diagradanl.obs[idxanl] - diagradanl.hx[idxanl]
    titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + imgtype

    vmin=min(oma_bc)
    vmax=max(oma_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	rp.plotglobal(m,oma_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)


    omb_bc = diagradges.obs[idxanl] - diagradges.hx[idxanl]
    titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + imgtype
    vmin=min(omb_bc)
    vmax=max(omb_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	rp.plotglobal(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)


    for i,val in enumerate(targetvars):

   		
        targetlat=targetlats[i]
        targetlon=targetlons[i]
        targetstr=targetstrs[i]


        if not os.path.isdir(targetstr): os.mkdir(targetstr)
        
        if targetlat < -80:
            m = Basemap(projection='splaea',boundinglat=-70,lon_0=90,resolution='l')
        else:
            m = Basemap(projection='stere',width=4800000,height=3600000,\
                                                resolution='c',\
                                                lon_0=targetlon,lat_0=targetlat)

        obsxpt,obsypt=m(obslons,obslats)
        obsanl = diagradanl.obs[idxges]

        if (obsanl.size > 0) :

            outfilename=targetstr+'/'+instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + '_' + \
                       val + '_' + str(targetlat) + '_' + str(targetlon) + imgtype
            listoftargets.append(outfilename)
            vmin=min(obsanl)
	    vmax=max(obsanl)

            anlobstargetimages[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,obsanl,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                        targetamps[i],targetlon,targetlat,outfilename)

            titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

            outfilenamebase=targetstr+'/'+instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + '_' + \
                         val + '_' + str(targetlat) + '_' + str(targetlon) 

#            tableline = '<tr><td><img src="'

	    vmin=-1.0
	    vmax=1.0
	    outfilename = outfilenamebase + '_fix' + imgtype
            omatargetimagesfix[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,oma_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

#            tableline = tableline + '"></td><td><img src="'

            vmin=min(oma_bc)
	    vmax=max(oma_bc)
#	    outfilename = outfilenamebase + '_rel.tiff'
	    outfilename = outfilenamebase + '_rel' + imgtype
#            tableline = tableline + outfilename
            omatargetimagesrel[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,oma_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

#            tableline = tableline +  '"></td></tr>\n'

#            tablelines[i].append(tableline)


        obsges = diagradges.obs[idxges]

        if (obsges.size > 0) :

       	    titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

            outfilename=targetstr+'/'+instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + '_' + \
                        val + '_' + str(targetlat) + '_' + str(targetlon) + imgtype

            vmin=min(obsges)
	    vmax=max(obsges)

            gesobstargetimages[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,obsges,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                        targetamps[i],targetlon,targetlat,outfilename)


            titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

            outfilenamebase=targetstr+'/'+instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + \
                            '_' + val + '_' + str(targetlat) + '_' + str(targetlon) 


	    vmin=-1.0
	    vmax=1.0
	    outfilename = outfilenamebase + '_fix' + imgtype
            ombtargetimagesfix[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)


            vmin=min(omb_bc)
	    vmax=max(omb_bc)
	    outfilename = outfilenamebase + '_rel' + imgtype
            ombtargetimagesrel[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

                
            amb_bc = omb_bc - oma_bc               
            titlestr = 'A-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

            outfilenamebase=targetstr+'/'+instrument+ '_ges_AmB_Ch%02d' % ichan + '_' + date + \
                            '_' + val + '_' + str(targetlat) + '_' + str(targetlon) 

	    vmin=-1.0
	    vmax=1.0
	    outfilename = outfilenamebase + '_fix' + imgtype
            ambtargetimagesfix[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,amb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)



            vmin=min(amb_bc)
	    vmax=max(amb_bc)
	    outfilename = outfilenamebase + '_rel' + imgtype
            ambtargetimagesrel[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(m,amb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)



pagelistfix=[]
pagelistrel=[]

for i,val in enumerate(targetvars):

    targetlat=targetlats[i]
    targetlon=targetlons[i]

    outfilename=instrument + '_OmA_OmB_' + date +  '_' + val + '_' + \
                 str(targetlat) + '_' + str(targetlon) + '_fix.html'

    
    if len(omatargetimagesfix[i]) > 0:
        with open(outfilename, "w") as f:
            print 'writing ' + outfilename
            pagelistfix.append(outfilename)
            header ='<html><head><title>title goes here</title></head><body>\n'
            f.write(header)
            f.write('<table border="1">\n')
#        for ichan in channels:
            for j in  range(len(omatargetimagesfix[i])):
#            for ichan in [0]:
                if omatargetimagesfix[i][j] :
                    f.write('<tr><td><img src="')
                    f.write(omatargetimagesfix[i][j])   
                    f.write('" width="500" ></td><td><img src="')
                    f.write(ombtargetimagesfix[i][j])   
                    f.write('" width="500" ></td><td><img src="')
                    f.write(ambtargetimagesfix[i][j])   
                    f.write('" width="500" ></td></tr>\n')
            f.write('</table></body></html>\n')

    outfilename=instrument + '_OmA_OmB_' + date +  '_' + val + '_' + \
                 str(targetlat) + '_' + str(targetlon) + '_rel.html'

    if len(omatargetimagesrel[i]) > 0:
        with open(outfilename, "w") as f:
            print 'writing ' + outfilename
            pagelistrel.append(outfilename)
            header ='<html><head><title>title goes here</title></head><body>\n'
            f.write(header)
            f.write('<table border="1">\n')
#        for ichan in channels:
            for j in  range(len(omatargetimagesrel[i])):
#            for ichan in [0]:
                if omatargetimagesrel[i][j] :
                    f.write('<tr><td><img src="')
                    f.write(omatargetimagesrel[i][j])   
                    f.write('" width="500" ></td><td><img src="')
                    f.write(ombtargetimagesrel[i][j])   
                    f.write('" width="500" ></td><td><img src="')
                    f.write(ambtargetimagesrel[i][j])   
                    f.write('" width="500" ></td></tr>\n')
            f.write('</table></body></html>\n')





originx=111 # 90N,180W
originy=60
farx=707 # 90S,180E
fary=496
xrange=farx-originx
yrange=fary-originy


#targetxpt,targetypt=m(targetlons,targetlats)
#maxxpt,maxypt=m(180,90)

with open('htmlmapareas',"w") as f:

    for i,val in enumerate(targetvars):

#        targetlat=90-targetlats[i] # convert 90:-90 to 0:180
#	targetlon=targetlons[i]+180 # convert -180:180 to 1:360



        ycoord=str(int(((maxypt-targetypt[i])/maxypt)*yrange)+originy)
        xcoord=str(int((targetxpt[i]/maxxpt)*xrange)+originx)

        latlonstr=str(targetlats[i]) + ',' + str(targetlons[i])

        pre='  <area shape="circle" coords="'
	f.write( pre + xcoord + ',' + ycoord + ',5" href="' + latlonstr + '.htm" alt="Venus">\n' )


pagelistfilename=instrument + '_OmB_and_OmA_' + date + '_fix.html'
with open(pagelistfilename, "w") as f:
    print 'writing ' + pagelistfilename
    header ='<html><head><title>Sample CGI Script</title></head><body>\n'
    f.write(header)
    f.write('<ul>\n')
    for i in range(len(pagelistfix)):
        f.write('<li><a href="' + pagelistfix[i] + '">' +  pagelistfix[i] + '</a>\n')

    f.write('</u></body></html>\n')

pagelistfilename=instrument + '_OmB_and_OmA_' + date + '_rel.html'
with open(pagelistfilename, "w") as f:
    print 'writing ' + pagelistfilename
    header ='<html><head><title>Sample CGI Script</title></head><body>\n'
    f.write(header)
    f.write('<ul>\n')
    for i in range(len(pagelistrel)):
        f.write('<li><a href="' + pagelistrel[i] + '">' +  pagelistrel[i] + '</a>\n')

    f.write('</u></body></html>\n')




#wb.makegepointmap(m,targetlats,targetlons,targetypt,targetxpt,listoftargets)

pagelistfilename=instrument + '_OmB_and_OmA_' + date + '_all.html'
#mapimage=date + '/' + instrument + '/' + instrument+'_ana_obs_Ch01_' + date + imgtype
mapimage= instrument+'_ana_obs_Ch01_' + date + imgtype

# the following are the pixels for the origin and kiddie corners of
# the global plots generated, determined empircally since the 
# transforms in matplotlib seem to be buggy.  An 800x600 jpg or 
# png image is assumed.
originx=111 # 90N,180W
originy=60
farx=707 # 90S,180E
fary=496
xrange=farx-originx
yrange=fary-originy
clicksize=5 # size of clickable area


with open(pagelistfilename, "w") as f:
    print 'writing ' + pagelistfilename
    header ='<html><head><title>Sample CGI Script</title></head><body>\n'
    f.write(header)
    imgmapline='<img src="' + mapimage + '" width="800" height="600" alt="Planets" usemap="#' + date + 'map">\n'
    f.write(imgmapline)
    f.write('<map name="' + date + 'map">\n')
    for i,val in enumerate(targetlats):

        ycoord=str(int(((maxypt-targetypt[i])/maxypt)*yrange)+originy)
        xcoord=str(int((targetxpt[i]/maxxpt)*xrange)+originx)

        latlonstr=str(targetlats[i]) + ',' + str(targetlons[i])
#        pagename=listoftargets[i]
        pagename=pagelistrel[i]

        pre='  <area shape="circle" coords="'
        f.write( pre + xcoord + ',' + ycoord + ',' + str(clicksize) )
        f.write( '" href="' + pagename + '" alt="Venus">\n' )



    f.write('</map>\n')
    f.write('<ul>\n')
    for i in range(len(pagelistrel)):
#        f.write('<li><a href="' + date + '/' + instrument + '/' + pagelistrel[i] + '">' +  pagelistrel[i] + '</a>\n')
        f.write('<li><a href="'  + pagelistrel[i] + '">' +  pagelistrel[i] + '</a>\n')
    f.write('</ul><ul>\n')
    for i in range(len(pagelistfix)):
#        f.write('<li><a href="' + date + '/' + instrument + '/' + pagelistfix[i] + '">' +  pagelistfix[i] + '</a>\n')
        f.write('<li><a href="'  + pagelistfix[i] + '">' +  pagelistfix[i] + '</a>\n')


    f.write('</ul></body></html>\n')





sys.exit()

