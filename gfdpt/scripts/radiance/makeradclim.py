#!/usrx/local/python/2.7.12/bin/python
import read_diag
import matplotlib
matplotlib.use('Agg')
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import numpy as np
import sys
import os
import itertools
from datetime import timedelta, date, datetime
import radplot as rp

date=2017112800
argdate=str(date)

# png for lossless
imgtype='.png'
plotimgwidth='640'
plotimgheight='480'

instrument='atms_npp'
instrument='amsua_n19'
instrument='amsua_n18'
instrument='amsua_n15'
instrument='amsua_metop-a'
instrument='amsua_metop-b'
#instrument='iasi_metop-a'
#instrument='airs_aqua'

stdfact = 2
allthethings = {}
lastdayvals = {}
allthestats = {}

if len(sys.argv) > 1:
    argdate = sys.argv[1]
    instrument = sys.argv[2]

argyear=int(argdate[0:4])
argmonth=int(argdate[4:6])
argday=int(argdate[6:8])
argcyc=int(argdate[8:10])

print argdate,argyear,argmonth,argday,argcyc


daycount=30
#daycount=5
#startdatetime=datetime(2017,11,29,00)
startdatetime=datetime(argyear,argmonth,argday,argcyc)
displaydate=startdatetime.strftime("%Y%m%d%H")

# llcrnrlat,llcrnrlon,urcrnrlat,urcrnrlon
# are the lat/lon values of the lower left and upper right corners
# of the map.

m = Basemap(projection='mill',llcrnrlat=-90,urcrnrlat=90,\
                                            llcrnrlon=-180,urcrnrlon=180)


if not os.path.isdir(displaydate): os.mkdir(displaydate)
os.chdir(displaydate)

if not os.path.isdir(instrument): os.mkdir(instrument)
os.chdir(instrument)




for cdatetime in (startdatetime - timedelta(n-1) for n in range(daycount,0,-1)):

    print cdatetime,instrument
    cdate=cdatetime.strftime("%Y%m%d%H")

    diagfiledir=os.environ['RADDATADIR'] + '/radstat/' + cdate + '/'

#    if not os.path.isdir(cdate): os.mkdir(cdate)
#    os.chdir(cdate)

#    if not os.path.isdir(instrument): os.mkdir(instrument)
#    os.chdir(instrument)


    # load diag files and look at stats

    diaganlfile = diagfiledir + 'diag_'+instrument+'_anl.'+cdate
    if os.path.isfile(diaganlfile):  
       print 'reading diag file ' + diaganlfile
       diagradanl = read_diag.diag_rad(diaganlfile,endian='big')
    else: 
       print 'skipping ' + diaganlfile
       continue

    diaggesfile = diagfiledir + 'diag_'+instrument+'_ges.'+cdate
    if os.path.isfile(diaganlfile):  
       print 'reading diag file ' + diaggesfile
       diagradges = read_diag.diag_rad(diaggesfile,endian='big')
    else: 
       print 'skipping ' + diaggesfile
       continue



    print 'total number anl obs = ',diagradanl.nobs
    diagradanl.read_obs()
    # print o-f stats for one channel

    print 'total number ges obs = ',diagradges.nobs
    diagradges.read_obs()
    # print o-f stats for one channel

    # assumes channels are same for both anl and ges
    channels= list(set(diagradanl.channel))


    # for testing
#    channels=[10]
    
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

        for i,val in enumerate(obslons):
            if val > 180.0  : obslons[i] = val - 360.0

        dec = 0
        obslats = [int(round(x,dec)) for x in obslats]
        obslons = [int(round(y,dec)) for y  in obslons]

#        locs=zip([int(round(x,dec)) for x in obslats],[int(round(y,dec)) for y \
#            in obslons], itertools.repeat(ichan) )
#        locs=zip([int(round(x,dec)) for x in obslats],[int(round(y,dec)) for y \
#            in obslons] )
        locs=zip(obslats,obslons)


        # analysis obs
        obsanl=diagradanl.obs[idxanl]

        # guess obs
        obsges=diagradges.obs[idxges]
        # O-A
        oma_bc = diagradanl.obs[idxanl] - diagradanl.hx[idxanl]
        # O-B
        omb_bc = diagradges.obs[idxanl] - diagradges.hx[idxanl]
        # A-B
        amb_bc = omb_bc - oma_bc # A-B = (O-B)-(O-A) = O - B - O + A
        
        vals = zip(obsanl,obsges,oma_bc,omb_bc,amb_bc)

        if ichan not in allthethings:
           allthethings.update({ichan:{}})

        for i, loc in enumerate(locs):
           if loc in allthethings[ichan]:
              allthethings[ichan][loc].append(vals[i])
           else:
              allthethings[ichan].update({loc:[vals[i]]})


        if cdatetime == startdatetime :
           print 'saving last day values'   

           if ichan not in lastdayvals:
              lastdayvals.update({ichan:{}})

           for i, loc in enumerate(locs):
#              if loc in lastdayvals[ichan]:
#                 print 'ping!'
#                 lastdayvals[ichan][loc].append(vals[i])
#              else:
#                 lastdayvals[ichan].update({loc:[vals[i]]})
# the above creates lists of those last day values, with some overlap - should probably be averaged
              lastdayvals[ichan].update({loc:vals[i]})


pagelistfilename='clim_' + instrument + '_' + argdate + '.html'
f=open(pagelistfilename,'w')
header ='<html><head><title>radiance climatology for ' + argdate + ' ' + instrument + '</title></head><body>\n'
f.write(header)


#for ichan in channels: 
for ichan, locvals in allthethings.items(): 
    print 'doing stats on channel ' + str(ichan)

    allthestats.update({ichan:{}})
    for loc,vals in allthethings[ichan].items():

        anls=[]
        gess=[]
        omas=[]
        ombs=[]
        ambs=[]

        nvals = len(vals)
        for val in vals:
            anls.append(val[0])
            gess.append(val[1])
            omas.append(val[2])
            ombs.append(val[3])
            ambs.append(val[4])

        locstat= nvals, np.mean(np.asarray(anls)), np.std(np.asarray(anls)),\
            np.mean(np.asarray(gess)),np.std(np.asarray(gess)), \
            np.mean(np.asarray(omas)),np.std(np.asarray(omas)), \
            np.mean(np.asarray(ombs)),np.std(np.asarray(ombs)), \
            np.mean(np.asarray(ambs)),np.std(np.asarray(ambs)) 

        allthestats[ichan].update({loc:locstat})

# get lists of the the lats/lons/all the stats

    statlocs = allthestats[ichan].keys()              
    statlats = [x[0] for x in statlocs]
    statlons = [y[1] for y in statlocs]
    obsxpt,obsypt=m(statlons,statlats)

    stats = allthestats[ichan].values()

# get latlons of last day's data to mask means for anomalies
    lastdaylocs = lastdayvals[ichan].keys()
    maskedlats = [x[0] for x in lastdaylocs]
    maskedlons = [y[1] for y in lastdaylocs]
    mskxpt,mskypt=m(maskedlons,maskedlats)

    maskedstats = [ allthestats[ichan][x] for x in lastdaylocs]
    lastdayvalslst = [ lastdayvals[ichan][x] for x  in lastdaylocs ]

# start current channel's group of images
    f.write('<table border="1">\n')
    f.write('<tr><td><img src="')

    titlesuffix =  displaydate + ' ' + instrument + ' Ch ' + str(ichan)

# number of points 
    npoints = [z[0] for z in stats ]
    titlestr='number of data'
    vmin = min(npoints)
    vmax = max(npoints)
    outfilename=instrument+'_N_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,npoints,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename,'data per point')
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td></tr>\n')


    f.write('<tr><td><img src="')
# anl mean
    anlavg = [z[1] for z in stats ]
    titlestr='anl 30-day mean ' +  titlesuffix
#    vmin = min(anlavg)
#    vmax = max(anlavg)
    vmin = np.mean(anlavg) - stdfact * np.std(anlavg)
    vmax = np.mean(anlavg) + stdfact * np.std(anlavg)
    outfilename=instrument+'_anl_avg_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,anlavg,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')


# anl std
    anlstd = [z[2] for z in stats ]
    titlestr='anl 30-day std ' + titlesuffix
#    vmin = min(anlstd)
#    vmax = max(anlstd)
#    vmin = np.mean(anlstd) - stdfact * np.std(anlstd)
    vmin = 0
    vmax = np.mean(anlstd) + stdfact * np.std(anlstd)
    outfilename=instrument+'_anl_std_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,anlstd,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')


# anl anomaly
    anlavg = [x[1] for x in maskedstats ]
    anl = [x[0] for x in lastdayvalslst ]
    anlanm = [ x1 - x2 for (x1,x2) in zip(anl,anlavg) ]

    titlestr='anl anomaly ' + titlesuffix
#    vmin = min(anlanm)
#    vmax = max(anlanm)
    vmin = np.mean(anlanm) - stdfact * np.std(anlanm)
    vmax = np.mean(anlanm) + stdfact * np.std(anlanm)
    outfilename=instrument+'_anl_anm_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,anlanm,mskxpt,mskypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td></tr>\n')

    f.write('<tr><td><img src="')
# ges mean
    gesavg = [z[3] for z in stats ]
    titlestr='ges 30-day mean ' +  titlesuffix
#    vmin = min(gesavg)
#    vmax = max(gesavg)
    vmin = np.mean(gesavg) - stdfact * np.std(gesavg)
    vmax = np.mean(gesavg) + stdfact * np.std(gesavg)
    outfilename=instrument+'_ges_avg_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,gesavg,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# ges std
    gesstd = [z[4] for z in stats ]
    titlestr='ges 30-day std ' +  titlesuffix
#    vmin = min(gesstd)
#    vmax = max(gesstd)
#    vmin = np.mean(gesstd) - stdfact * np.std(gesstd)
    vmin = 0
    vmax = np.mean(gesstd) + stdfact * np.std(gesstd)
    outfilename=instrument+'_ges_std_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,gesstd,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# ges anomaly
    gesavg = [x[3] for x in maskedstats ]
    ges = [x[1] for x in lastdayvalslst ]
    gesanm = [ x1 - x2 for (x1,x2) in zip(ges,gesavg) ]

    titlestr='ges anomaly ' +  titlesuffix
#    vmin = min(gesanm)
#    vmax = max(gesanm)
    vmin = np.mean(gesanm) - stdfact * np.std(gesanm)
    vmax = np.mean(gesanm) + stdfact * np.std(gesanm)
    outfilename=instrument+'_ges_anm_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,gesanm,mskxpt,mskypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td></tr>\n')

    f.write('<tr><td><img src="')
# oma mean
    omaavg = [z[5] for z in stats ]
    titlestr='O-A 30-day mean ' +  titlesuffix
#    vmin = min(omaavg)
#    vmax = max(omaavg)
    vmin = np.mean(omaavg) - stdfact * np.std(omaavg)
    vmax = np.mean(omaavg) + stdfact * np.std(omaavg)
    outfilename=instrument+'_oma_avg_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,omaavg,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# oma std
    omastd = [z[6] for z in stats ]
    titlestr='O-A 30-day std ' +  titlesuffix
#    vmin = min(omastd)
#    vmax = max(omastd)
#    vmin = np.mean(omastd) - stdfact * np.std(omastd)
    vmin = 0
    vmax = np.mean(omastd) + stdfact * np.std(omastd)
    outfilename=instrument+'_oma_std_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,omastd,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# oma anomaly
    omaavg = [x[5] for x in maskedstats ]
    oma = [x[2] for x in lastdayvalslst ]
    omaanm = [ x1 - x2 for (x1,x2) in zip(oma,omaavg) ]

    titlestr='O-A anomaly ' +  titlesuffix
#    vmin = min(omaanm)
#    vmax = max(omaanm)
    vmin = np.mean(omaanm) - stdfact * np.std(omaanm)
    vmax = np.mean(omaanm) + stdfact * np.std(omaanm)
    outfilename=instrument+'_oma_anm_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,omaanm,mskxpt,mskypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td></tr>\n')


    f.write('<tr><td><img src="')
# omb mean
    ombavg = [z[7] for z in stats ]
    titlestr='O-B 30-day mean ' +  titlesuffix
#    vmin = min(ombavg)
#    vmax = max(ombavg)
    vmin = np.mean(ombavg) - stdfact * np.std(ombavg)
    vmax = np.mean(ombavg) + stdfact * np.std(ombavg)
    outfilename=instrument+'_omb_avg_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,ombavg,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# omb std
    ombstd = [z[8] for z in stats ]
    titlestr='O-B 30-day std ' +  titlesuffix
#    vmin = min(ombstd)
#    vmax = max(ombstd)
#    vmin = np.mean(ombstd) - stdfact * np.std(ombstd)
    vmin = 0
    vmax = np.mean(ombstd) + stdfact * np.std(ombstd)
    outfilename=instrument+'_omb_std_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,ombstd,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# omb anomaly
    ombavg = [x[7] for x in maskedstats ]
    omb = [x[3] for x in lastdayvalslst ]
    ombanm = [ x1 - x2 for (x1,x2) in zip(omb,ombavg) ]

    titlestr='O-B anomaly ' +  titlesuffix
#    vmin = min(ombanm)
#    vmax = max(ombanm)
    vmin = np.mean(ombanm) - stdfact * np.std(ombanm)
    vmax = np.mean(ombanm) + stdfact * np.std(ombanm)
    outfilename=instrument+'_omb_anm_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,ombanm,mskxpt,mskypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td></tr>\n')


    f.write('<tr><td><img src="')
# A-B mean
    ambavg = [z[9] for z in stats ]
    titlestr='A-B 30-day mean ' +  titlesuffix
#    vmin = min(ambavg)
#    vmax = max(ambavg)
    vmin = np.mean(ambavg) - stdfact * np.std(ambavg)
    vmax = np.mean(ambavg) + stdfact * np.std(ambavg)
    outfilename=instrument+'_amb_avg_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,ambavg,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# A-B std
    ambstd = [z[10] for z in stats ]
    titlestr='A-B 30-day std ' +  titlesuffix
#    vmin = min(ambstd)
#    vmax = max(ambstd)
#    vmin = np.mean(ambstd) - stdfact * np.std(ambstd)
    vmin = 0
    vmax = np.mean(ambstd) + stdfact * np.std(ambstd)
    outfilename=instrument+'_amb_std_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,ambstd,obsxpt,obsypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td><td><img src="')

# A-B anomaly
    ambavg = [x[9] for x in maskedstats ]
    amb = [x[4] for x in lastdayvalslst ]
    ambanm = [ x1 - x2 for (x1,x2) in zip(amb,ambavg) ]

    titlestr='A-B anomaly ' +  titlesuffix
#    vmin = min(ambanm)
#    vmax = max(ambanm)
    vmin = np.mean(ambanm) - stdfact * np.std(ambanm)
    vmax = np.mean(ambanm) + stdfact * np.std(ambanm)
    outfilename=instrument+'_amb_anm_Ch%02d' % ichan + '_' + displaydate + imgtype
    rp.plotglobal(m,ambanm,mskxpt,mskypt,titlestr,vmin,vmax,[],[],[],[],outfilename)
    f.write(outfilename)
    f.write('" width="' + plotimgwidth + '" height="' + plotimgheight + '" ></td></tr>\n')

    f.write('</table></body></html>\n')

f.close()
sys.exit()

