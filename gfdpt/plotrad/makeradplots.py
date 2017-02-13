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
#date='2015092600'
#date='2015101700'
#date='2015101800'
#date='2015121000'
#date='2016071100'


diagfiledir='/data/users/aeichmann/COATsvn/radstat/'

instrument='atms_npp'
#instrument='amsua_n19'
#instrument='amsua_n18'

makeplots=True
#makeplots=False

# png for lossless
imgtype='.png'

# set up dictionary of atms/amsua channel heights
channelheights= { 1 : '(wnd-H2O vpr)',
		  2 : '(wnd-H2O vpr)',
		  3 : '(wnd-srfc)',
		  4 : '(wnd-srfc)',
		  5 : '(srfc air)',
		  6 : '(700mb)',
		  7 : '(400mb)',
		  8 : '(250mb)',
		  9 : '(180mb)',
		 10 : '(90mb)',
		 11 : '(50mb)',
		 12 : '(25mb)',
		 13 : '(10mb)',
		 14 : '(6mb)',
		 15 : '(3mb)',
		 16 : '(wnd H20)',
		 17 : '(H20 18mm)',
		 18 : '(H20 18mm)',
		 19 : '(H20 4.5mm)',
		 20 : '(H20 2.5mm)',
		 21 : '(H20 1.2mm)',
		 22 : '(H20 0.5mm)' }

if len(sys.argv) > 1:
    date = sys.argv[1]
    instrument = sys.argv[2]

print date,instrument

# location of gribextremes output
gefile = '/data/users/aeichmann/gfdpt-checkout/gfdpt/gribextremes/run'+date+'/fort.55'

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

print 'targetvars: ', targetvars
print 'targetlats: ', targetlats
print 'targetlons: ', targetlons
print 'targetamps: ', targetamps
print 'targetstrs: ', targetstrs

#sys.exit()

listoftargets=[]

# load diag files and look at stats

diaganlfile = diagfiledir + 'diag_'+instrument+'_anl.'+date
diagradanl = read_diag.diag_rad(diaganlfile,endian='big')

diaggesfile = diagfiledir + 'diag_'+instrument+'_ges.'+date
diagradges = read_diag.diag_rad(diaggesfile,endian='big')


#sys.exit()
print 'total number anl obs = ',diagradanl.nobs
diagradanl.read_obs()
# print o-f stats for one channel

print 'total number ges obs = ',diagradges.nobs
diagradges.read_obs()
# print o-f stats for one channel

# assumes channels are same for both anl and ges
channels= list(set(diagradanl.channel))

# llcrnrlat,llcrnrlon,urcrnrlat,urcrnrlon
# are the lat/lon values of the lower left and upper right corners
# of the map.

m = Basemap(projection='mill',llcrnrlat=-90,urcrnrlat=90,\
		                            llcrnrlon=-180,urcrnrlon=180)

# get corners in projection for use later
maxxpt,maxypt=m(180,90)

rnbw = cm = plt.get_cmap('rainbow') 

# shift lons to -180:180
for i,val in enumerate(targetlons):
     if val > 180.0  : targetlons[i] = val - 360.0

# get targets coords in projection
targetxpt,targetypt=m(targetlons,targetlats)




# set up lists for image filenames
omatargetimagesfix=[[] for x in xrange(numgepoints)]
ombtargetimagesfix=[[] for x in xrange(numgepoints)]
ambtargetimagesfix=[[] for x in xrange(numgepoints)]
omatargetimagesrel=[[] for x in xrange(numgepoints)]
ombtargetimagesrel=[[] for x in xrange(numgepoints)]
ambtargetimagesrel=[[] for x in xrange(numgepoints)]
anlobstargetimages=[[] for x in xrange(numgepoints)]
gesobstargetimages=[[] for x in xrange(numgepoints)]
qctargetimages=[[] for x in xrange(numgepoints)]

omaglobalimages=[]
ombglobalimages=[]
ambglobalimages=[]
anlobsglobalimages=[]
gesobsglobalimages=[]
qcglobalimages=[]



# for testing
#channels=[6]

for ichan in channels:

   # get indices of all matching channels
    idxallanl = diagradanl.channel == ichan
    idxallges = diagradges.channel == ichan
    # how many obs total
    nobsallanl = idxallanl.sum()
    nobsallges = idxallges.sum()
    # get indices of used obs with matching channels

    # find every point used in both anl and ges
    idxanl = np.logical_and(np.logical_and(np.logical_and(diagradanl.channel == ichan, \
            diagradanl.used == 1), diagradanl.oberr < 1.e9), diagradges.oberr < 1.e9)

    nobsanl = idxanl.sum()
    idxges = np.logical_and(np.logical_and(np.logical_and(diagradges.channel == ichan, \
            diagradges.used == 1), diagradges.oberr < 1.e9), diagradanl.oberr < 1.e9)
    nobsges = idxges.sum()

    # these should be same
    if nobsges != nobsanl:
        print 'nobsges = ' + str(nobsges) + ', nobsanl = ' + str(nobsanl)
        sys.exit()

    # skip channel if no obs used
    if nobsanl == 0: continue 

    fitsq = ((diagradanl.hx[idxanl]-diagradanl.obs[idxanl])**2).mean()

#    print diagradanl.obs[6],diagradanl.hx[6],diagradanl.biascorr[6],diagradanl.biaspred[1:,6].sum()
    print nobsanl,'anl: obs used for channel',ichan,'out of',nobsallanl,'rms o-f', np.sqrt(fitsq)

#    print diagradges.obs[6],diagradges.hx[6],diagradges.biascorr[6],diagradges.biaspred[1:,6].sum()
    print nobsges,'ges: obs used for channel',ichan,'out of',nobsallges,'rms o-f', np.sqrt(fitsq)

    obslats=diagradanl.lat[idxanl]
    obslons=diagradanl.lon[idxanl]

#    print diagradanl.qcmark[idxallanl]
#    sys.exit()

    # these should be same
    if any(obslats != diagradges.lat[idxges]) or any(obslons != diagradges.lon[idxges]):
        print 'skronk'

    if instrument == 'atms_npp':
        channelheight=channelheights[ichan]
    elif instrument[0:3] == 'amsu':
	if ichan < 4:
           channelheight=channelheights[ichan]
	else:
           channelheight=channelheights[ichan+1]
    else: channelheight=''	   
           		
	   




    for i,val in enumerate(obslons):
        if val > 180.0  : obslons[i] = val - 360.0

    obsxpt,obsypt=m(obslons,obslats)
    
    # commence to plotting global fields

    # analysis obs
    field=diagradanl.obs[idxanl]
    anlobsstd=np.std(field)
    titlestr = 'Tb ana ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + imgtype
    anlobsglobalimages.append(outfilename)
#    vmin=min(field)
#    vmax=max(field)
    vmin=np.mean(field)-anlobsstd*3
    vmax=np.mean(field)+anlobsstd*3
    if makeplots:
    	print 'generating ' + outfilename 
	rp.plotglobal(m,field,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)

    # guess obs
    field=diagradges.obs[idxges]
    gesobsstd=np.std(field)
    titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan)
    outfilename=instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + imgtype
    gesobsglobalimages.append(outfilename)
#    vmin=min(field)
#    vmax=max(field)
    vmin=np.mean(field)-gesobsstd*3
    vmax=np.mean(field)+gesobsstd*3
    if makeplots:
    	print 'generating ' + outfilename 
	rp.plotglobal(m,field,obsxpt,obsypt,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)

    # O-A
    oma_bc = diagradanl.obs[idxanl] - diagradanl.hx[idxanl]
    omastd=np.std(oma_bc)
#    vminoma=np.mean(oma_bc)-omastd
#    vmaxoma=np.mean(oma_bc)+omastd
    vminoma=-omastd*3
    vmaxoma=omastd*3
    titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + channelheight
    outfilename=instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + imgtype
    omaglobalimages.append(outfilename)

#    vmin=min(oma_bc)
#    vmax=max(oma_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	rp.plotglobal(m,oma_bc,obsxpt,obsypt,titlestr,vminoma,vmaxoma,targetvars,targetamps,targetxpt,targetypt,outfilename)

    # O-B
    omb_bc = diagradges.obs[idxanl] - diagradges.hx[idxanl]
    ombstd=np.std(omb_bc)
#    print 'ombstd: ',ombstd
#    plt.hist(omb_bc)
#    plt.show()
#    vminomb=np.mean(omb_bc)-ombstd
#    vmaxomb=np.mean(omb_bc)+ombstd
    vminomb=-ombstd*3
    vmaxomb=+ombstd*3
#    print 'vmin: ', vmin
#    print 'vmax: ',vmax
    titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + channelheight
    outfilename=instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + imgtype
    ombglobalimages.append(outfilename)
#    vmin=min(omb_bc)
#    vmax=max(omb_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	rp.plotglobal(m,omb_bc,obsxpt,obsypt,titlestr,vminomb,vmaxomb,targetvars,targetamps,targetxpt,targetypt,outfilename)

    # A-B
    amb_bc = omb_bc - oma_bc # A-B = (O-B)-(O-A) = O - B - O + A
    ambstd=np.std(amb_bc)
#    vminamb=np.mean(amb_bc)-ambstd
#    vmaxamb=np.mean(amb_bc)+ambstd
    vminamb=-ambstd*3
    vmaxamb=+ambstd*3
    titlestr = 'A-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + channelheight
    outfilename=instrument+ '_AmB_Ch%02d' % ichan + '_' + date + imgtype
    ambglobalimages.append(outfilename)
#    vmin=min(amb_bc)
#    vmax=max(amb_bc)
    if makeplots:
    	print 'generating ' + outfilename 
    	rp.plotglobal(m,amb_bc,obsxpt,obsypt,titlestr,vminamb,vmaxamb,targetvars,targetamps,targetxpt,targetypt,outfilename)


    titlestr = 'QC flag ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + channelheight
    outfilename=instrument+ '_QC_Ch%02d' % ichan + '_' + date + imgtype
    qcglobalimages.append(outfilename)
#    ambglobalimages.append(outfilename)
#    vmin=min(qcmark)
#    vmax=max(qcmark)
    if makeplots:
    	print 'generating ' + outfilename 
#    	rp.plotglobal(m,qcmark,obsxptall,obsyptall,titlestr,vmin,vmax,targetvars,targetamps,targetxpt,targetypt,outfilename)
        rp.plotglobalqc(m,diagradanl,ichan,titlestr,targetvars,targetamps,targetxpt,targetypt,outfilename)




# now do the same zoomed in on each of the gribextremes points
    for i,val in enumerate(targetvars):
#    for i,val in []:

        targetlat=targetlats[i]
        targetlon=targetlons[i]
        targetstr=targetstrs[i]


        if not os.path.isdir(targetstr): os.mkdir(targetstr)
       
       # use different projections for near the poles
        if targetlat < -80:
            mtgt = Basemap(projection='splaea',boundinglat=-70,lon_0=90,resolution='l')
        else:
            mtgt = Basemap(projection='stere',width=4800000,height=3600000,\
                                                resolution='c',\
                                                lon_0=targetlon,lat_0=targetlat)

        # get obs coords in local projection
        obsxpt,obsypt=mtgt(obslons,obslats)
        obsanl = diagradanl.obs[idxges]

        if (obsanl.size > 0) :


            if 0:		
               # plot anl obs
               outfilename=targetstr+'/'+instrument+'_ana_obs_Ch%02d' % ichan + '_' + date + '_' + \
                          val + '_' + str(targetlat) + '_' + str(targetlon) + imgtype
               listoftargets.append(outfilename)
               vmin=min(obsanl)
     	       vmax=max(obsanl)

               anlobstargetimages[i].append(outfilename)
               if makeplots:
                   print 'generating ' + outfilename 
                   rp.plottarget(mtgt,obsanl,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                          targetamps[i],targetlon,targetlat,outfilename)

            # plot O-A
            titlestr = 'O-A (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])
            outfilenamebase=targetstr+'/'+instrument+'_ana_OmA_Ch%02d' % ichan + '_' + date + '_' + \
                         val + '_' + str(targetlat) + '_' + str(targetlon) 
	    vmin=-1.0
	    vmax=1.0
	    vmin=vminoma
	    vmax=vmaxoma
	    outfilename = outfilenamebase + '_fix' + imgtype
            omatargetimagesfix[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(mtgt,oma_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

            vmin=min(oma_bc)
	    vmax=max(oma_bc)
	    vmin=vminoma
	    vmax=vmaxoma
	    outfilename = outfilenamebase + '_rel' + imgtype
            omatargetimagesrel[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(mtgt,oma_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)


        obsges = diagradges.obs[idxges]

        if (obsges.size > 0) :

            if 0:
                # plot ges obs
          	titlestr = 'Tb ges ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

                outfilename=targetstr+'/'+instrument+'_ges_obs_Ch%02d' % ichan + '_' + date + '_' + \
                        val + '_' + str(targetlat) + '_' + str(targetlon) + imgtype

                vmin=min(obsges)
	        vmax=max(obsges)

                gesobstargetimages[i].append(outfilename)
                if makeplots:
                    print 'generating ' + outfilename 
                    rp.plottarget(mtgt,obsges,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                            targetamps[i],targetlon,targetlat,outfilename)


            # plot O-B
            titlestr = 'O-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

            outfilenamebase=targetstr+'/'+instrument+ '_ges_OmB_Ch%02d' % ichan + '_' + date + \
                            '_' + val + '_' + str(targetlat) + '_' + str(targetlon) 


	    vmin=-1.0
	    vmax=1.0
	    vmin=vminomb
	    vmax=vmaxomb
	    outfilename = outfilenamebase + '_fix' + imgtype
            ombtargetimagesfix[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(mtgt,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)


            vmin=min(omb_bc)
	    vmax=max(omb_bc)
	    vmin=vminomb
	    vmax=vmaxomb
	    outfilename = outfilenamebase + '_rel' + imgtype
            ombtargetimagesrel[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(mtgt,omb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

            # plot A-B = (O-B)-(O-A) = O - B - O + A      
            amb_bc = omb_bc - oma_bc               
            titlestr = 'A-B (w/ bc) ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + val + ' ' + str(targetamps[i])

            outfilenamebase=targetstr+'/'+instrument+ '_ges_AmB_Ch%02d' % ichan + '_' + date + \
                            '_' + val + '_' + str(targetlat) + '_' + str(targetlon) 

	    vmin=-1.0
	    vmax=1.0
	    vmin=vminamb
	    vmax=vmaxamb
	    outfilename = outfilenamebase + '_fix' + imgtype
            ambtargetimagesfix[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(mtgt,amb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)



            vmin=min(amb_bc)
	    vmax=max(amb_bc)
	    vmin=vminamb
	    vmax=vmaxamb
	    outfilename = outfilenamebase + '_rel' + imgtype
            ambtargetimagesrel[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottarget(mtgt,amb_bc,obsxpt,obsypt,titlestr,vmin,vmax,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)

            # plot A-B = (O-B)-(O-A) = O - B - O + A      
            titlestr = 'QC flag ' + date + ' ' + instrument + ' Ch ' + str(ichan) + ' ' + channelheight+ ' ' + val + ' ' + str(targetamps[i])
            outfilename=targetstr+'/'+instrument+ '_QC_Ch%02d' % ichan + '_' + date +'_' + val + '_' + str(targetlat) + '_' + str(targetlon) + imgtype

            qctargetimages[i].append(outfilename)
            if makeplots:
                print 'generating ' + outfilename 
                rp.plottargetqc(mtgt,diagradanl,ichan,titlestr,targetvars[i], \
                    targetamps[i],targetlon,targetlat,outfilename)



# create html pages point to zoomed images
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
            for j in  range(len(omatargetimagesfix[i])):
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
#    imgmapline='<img src="' + mapimage + '" width="800" height="600" alt="Planets" usemap="#' + date + instrument + '">\n'
    imgmapline='<img src="' + mapimage + '" width="800" height="600" usemap="#' + date + instrument + '">\n'
    f.write(imgmapline)
    f.write('<map name="' + date + instrument + '">\n')
    for i,val in enumerate(targetlats):

        ycoord=str(int(((maxypt-targetypt[i])/maxypt)*yrange)+originy)
        xcoord=str(int((targetxpt[i]/maxxpt)*xrange)+originx)

        latlonstr=str(targetlats[i]) + ',' + str(targetlons[i])
#        pagename=listoftargets[i]
        pagename=pagelistrel[i]

        pre='  <area shape="circle" coords="'
        f.write( pre + xcoord + ',' + ycoord + ',' + str(clicksize) )
#        f.write( '" href="' + pagename + '" alt="Venus">\n' )
        f.write( '" href="' + pagename + '" >\n' )



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




# create html file showing O-A, O-B, anl obs, and A-B for each date and instrument
pagelistfilename=instrument + '_' + date + '.html'

with open(pagelistfilename, "w") as f:
    print 'writing ' + pagelistfilename
    header ='<html><head><title>radiance data for ' + date + ' ' + instrument + '</title></head><body>\n'
    f.write(header)

    for i in  range(len(omaglobalimages)):
        f.write('<table border="1">\n')
        f.write('<tr><td><img src="')
        f.write(omaglobalimages[i])   
        f.write('" width="800" height="600" usemap="#' + date + instrument + '"></td><td><img src="')
        f.write(ombglobalimages[i])   
        f.write('" width="800" height="600" usemap="#' + date + instrument + '"></td></tr>\n')
        f.write('<tr><td><img src="')
#        f.write(anlobsglobalimages[i])   
        f.write(qcglobalimages[i])   
        f.write('" width="800" height="600" usemap="#' + date + instrument + '"></td><td><img src="')
        f.write(ambglobalimages[i])   
        f.write('" width="800" height="600" usemap="#' + date + instrument + '" ></td></tr>\n')
        f.write('</table></body></html>\n')
        f.write('<hr>\n')

    f.write('<map name="' + date + instrument + '">\n')

    for i,val in enumerate(targetlats):

        ycoord=str(int(((maxypt-targetypt[i])/maxypt)*yrange)+originy)
        xcoord=str(int((targetxpt[i]/maxxpt)*xrange)+originx)

        latlonstr=str(targetlats[i]) + ',' + str(targetlons[i])
        pagename=pagelistrel[i]

        pre='  <area shape="circle" coords="'
        f.write( pre + xcoord + ',' + ycoord + ',' + str(clicksize) )
        f.write( '" href="' + pagename + '" >\n' )

    f.write('</map>\n')
    f.write('<ul>\n')
    for i in range(len(pagelistrel)):
        f.write('<li><a href="'  + pagelistrel[i] + '">' +  pagelistrel[i] + '</a>\n')
    f.write('</ul><ul>\n')
    for i in range(len(pagelistfix)):
        f.write('<li><a href="'  + pagelistfix[i] + '">' +  pagelistfix[i] + '</a>\n')

    f.write('</ul></body></html>\n')





sys.exit()

