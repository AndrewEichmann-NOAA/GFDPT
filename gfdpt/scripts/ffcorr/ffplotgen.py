######!/usr/bin/python
import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import sys
import math
from datetime import timedelta, date, datetime


def \
ffplot(ff00Z,ff12Z,vdates,var,pheight,region,numdays,fcstday,stat,mean00Z,mean12Z,std00Z,std12Z):

    yfile='YELLOW.txt'
    rfile='RED.txt'

    coralertnhfile='COR_ALERT_NH.txt'
    coralertshfile='COR_ALERT_SH.txt'
    rmsalertnhfile='RMS_ALERT_NH.txt'
    rmsalertshfile='RMS_ALERT_SH.txt'

    alertfile='BAD_FILE.txt'

    YELLOW=False
    RED=False

    graphdays=numdays+fcstday+1
#    vdates=vdates[-numdays:]
    vdates=vdates[-graphdays:]
    vdates12z=[date - timedelta(hours=12) for date in vdates]

#    locs=range(0,len(vdates),5)
    locs=vdates[0:-1:5]
#    labels=[vdates[int(x)].strftime("%Y%m%d" ) for x in locs]
    labels=[date.strftime("%Y%m%d" ) for date in locs]

    ff00Z=np.array(ff00Z)
    ff12Z=np.array(ff12Z)
    mean00Z=np.array(mean00Z)
    mean12Z=np.array(mean12Z)
    std00Z=np.array(std00Z)
    std12Z=np.array(std12Z)

    # some relation values are -99.0, make them nans
    ff00Z[np.where(ff00Z < -1.0 )]=np.NAN
    ff12Z[np.where(ff12Z < -1.0 )]=np.NAN

#    ff00Zg=ff00Z[-graphdays:]
#    ff12Zg=ff12Z[-graphdays:]

    if stat == 'cor' :

        ff00Zsig1= mean00Z - std00Z 
        ff00Zsig2= mean00Z - 2*std00Z 
        ff00Zsig3= mean00Z - 3*std00Z 

        ff12Zsig1= mean12Z - std12Z 
        ff12Zsig2= mean12Z - 2*std12Z 
        ff12Zsig3= mean12Z - 3*std12Z 


        if ff00Z[-1] < ff00Zsig3[-1]:
            RED=True
            with open(rfile, 'a') as f:
                f.write( '00Z cor ' + str(ff00Z[-1]) + ' < 2sig ' + \
                        str(ff00Zsig3[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' ) 

        elif ff00Z[-1] < ff00Zsig2[-1]:
            YELLOW=True
            with open(yfile, 'a') as f:
                f.write( '00Z cor ' + str(ff00Z[-1]) + ' < 1sig ' + \
                        str(ff00Zsig2[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' )

        if ff12Z[-1] < ff12Zsig3[-1]:
            RED=True
            with open(rfile, 'a') as f:
                f.write( '12Z cor ' + str(ff12Z[-1]) + ' < 2sig ' + \
                        str(ff12Zsig3[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' )

        elif ff12Z[-1] < ff12Zsig2[-1]:
            YELLOW=True
            with open(yfile, 'a') as f:
                f.write( '12Z cor ' + str(ff12Z[-1]) + ' < 1sig ' + \
                        str(ff12Zsig2[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' )

        if var == 'HGT' and pheight == '500' and fcstday == 5:

            if RED: alertstatus = 'RED\n'
            elif YELLOW: alertstatus = 'YELLOW\n'
            else: alertstatus = 'NONE\n'

            if region == 'NH': alertfile = coralertnhfile
            elif region == 'SH': alertfile = coralertshfile
            else: print ('bad region value' + region)
            
            with open(alertfile, 'w') as f:
                f.write( alertstatus)
 
    elif stat == 'rms' :

        ff00Zsig1= mean00Z + std00Z 
        ff00Zsig2= mean00Z + 2*std00Z 
        ff00Zsig3= mean00Z + 3*std00Z 

        ff12Zsig1= mean12Z + std12Z 
        ff12Zsig2= mean12Z + 2*std12Z 
        ff12Zsig3= mean12Z + 3*std12Z 


        if ff00Z[-1] > ff00Zsig3[-1]:
            RED=True
            with open(rfile, 'a') as f:
                f.write( '00Z rms ' + str(ff00Z[-1]) + ' > 2sig ' + \
                        str(ff00Zsig3[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' ) 

        elif ff00Z[-1] > ff00Zsig2[-1]:
            YELLOW=True
            with open(yfile, 'a') as f:
                f.write( '00Z rms ' + str(ff00Z[-1]) + ' > 1sig ' + \
                        str(ff00Zsig2[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' )

        if ff12Z[-1] > ff12Zsig3[-1]:
            RED=True
            with open(rfile, 'a') as f:
                f.write( '12Z rms ' + str(ff12Z[-1]) + ' > 2sig ' + \
                        str(ff12Zsig3[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' )

        elif ff12Z[-1] > ff12Zsig2[-1]:
            YELLOW=True
            with open(yfile, 'a') as f:
                f.write( '12Z rms ' + str(ff12Z[-1]) + ' > 1sig ' + \
                        str(ff12Zsig2[-1]) + ' ' + str(var) + ' P' + \
                        str(pheight) + ' ' + region + ' day ' + \
                        str(fcstday) + '\n' )

        if var == 'HGT' and pheight == '500' and fcstday == 5:

            if RED: alertstatus = 'RED\n'
            elif YELLOW: alertstatus = 'YELLOW\n'
            else: alertstatus = 'NONE\n'

            if region == 'NH': alertfile = rmsalertnhfile
            elif region == 'SH': alertfile = rmsalertshfile
            else: print( 'bad region value' + region)
 
            with open(alertfile, 'w') as f:
                f.write( alertstatus)
 
    else:
        print( 'something  wrong')
        sys.exit()


    yaxis00z = range(len(vdates))

    plt.figure(figsize=(8,6))
#    plt.plot(vdates,ff00Zg,linestyle='-',color='k',marker='o',markerfacecolor='w',label='00Z')
    plt.plot(vdates,ff00Z,linestyle='-',color='k',marker='o',markerfacecolor='w',label='00Z')
    plt.plot(vdates,ff00Zsig2,linestyle='-.',color='k',marker='',markerfacecolor='w',\
            label='00Z 30d 2sig')
    plt.plot(vdates,ff00Zsig3,linestyle='--',color='k',marker='',markerfacecolor='w',\
            label='00Z 30d 3sig')
#    plt.plot(vdates12z,ff12Zg,linestyle=':',color='r',marker='v',markeredgecolor='r',markerfacecolor='w',label='12Z')
    plt.plot(vdates12z,ff12Z,linestyle=':',color='r',marker='v',markeredgecolor='r',markerfacecolor='w',label='12Z')
    plt.plot(vdates12z,ff12Zsig2,linestyle='-.',color='r',marker='',markerfacecolor='w',\
            label='12Z 30d 2sig')
    plt.plot(vdates12z,ff12Zsig3,linestyle='--',color='r',marker='',markerfacecolor='w',\
            label='12Z 30d 3sig')

# add blue vertical line indicating the current (intialization) day
    plt.axvline(x=vdates[-1]-timedelta(days=fcstday))


    plt.title('F-F ' + stat +': ' + var + ' ' + pheight + ' GX/' + region + \
            ' GFS, Day ' + str(fcstday))

#    minval= min(np.nanmin(ff00Zsig3),np.nanmin(ff00Zsig2),np.nanmin(ff00Zg),np.nanmin(ff12Zg))
#    print 'minvals: ',np.nanmin(ff00Zsig3),np.nanmin(ff00Zsig2),np.nanmin(ff00Zg),np.nanmin(ff12Zg)
#    maxval= max(np.nanmax(ff00Zsig3),np.nanmax(ff00Zsig2),np.nanmax(ff00Zg),np.nanmax(ff12Zg))
#    print 'maxvals: ',np.nanmax(ff00Zsig3),np.nanmax(ff00Zsig2),np.nanmax(ff00Zg),np.nanmax(ff12Zg)
    minval= min(np.nanmin(ff00Zsig3),np.nanmin(ff00Zsig2),np.nanmin(ff00Z),np.nanmin(ff12Z))
    print ('minvals: ',np.nanmin(ff00Zsig3),np.nanmin(ff00Zsig2),np.nanmin(ff00Z),np.nanmin(ff12Z))
    maxval= max(np.nanmax(ff00Zsig3),np.nanmax(ff00Zsig2),np.nanmax(ff00Z),np.nanmax(ff12Z))
    print( 'maxvals: ',np.nanmax(ff00Zsig3),np.nanmax(ff00Zsig2),np.nanmax(ff00Z),np.nanmax(ff12Z))
    if stat == 'cor' :
        graphmin=minval-0.4*(1.0-minval)
        graphmax=1.0
    elif stat == 'rms' :
        graphmin=minval-0.5*(maxval-minval)
        graphmax=maxval+0.2*(maxval-minval)
# nans make bad axis limits
        if math.isnan(graphmax): graphmax = 100
        print( 'rms: ',maxval, graphmax,minval,graphmin)
    else:
        print( 'something  wrong')
        sys.exit()

# nans make bad axis limits
    if math.isnan(graphmin): graphmin = 0

    print('graphmin: ', graphmin,', graphmax: ',graphmax)
    plt.axis([vdates12z[0]-timedelta(days=1),vdates[-1]+timedelta(days=1),graphmin,graphmax])
    plt.xticks(locs,labels,rotation=10)
    plt.xticks(rotation=10)
    plt.xlabel('valid date')
    plt.legend(loc=3, ncol=2) # lower left
    plt.grid()
    outfilename='fcst' + stat + '_' + var + '_P' + pheight + '_' + region + '_day' + str(fcstday) + '.png'
    print( 'saving ' + outfilename)
    plt.savefig(outfilename)
    #plt.show()
    plt.close('all')
    
    return (RED,YELLOW)
    

   
