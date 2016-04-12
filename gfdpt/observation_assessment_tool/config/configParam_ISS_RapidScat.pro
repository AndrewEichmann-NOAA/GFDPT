;---------------------------------------------------------------------------------
; Name:  configParam_ISS_RapidScat.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for ISS_RapidScat
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Eric S. Maddy (RTI) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: Feb, 5 2015, EMaddy, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_ISS_RapidScat, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV  = 1000000L
   paramStruct.MAX_CHAN = 2L 

   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = 'not.set'
   paramStruct.radListFile2 = 'not.set'
   ; Bias and standard deviation files (ISS_RapidScat not needed)
   paramStruct.biasListFile   = 'not.set'
   paramStruct.stddevListFile = 'not.set'
   ; cloud-liquid water regressions path (ISS_RapidScat not needed)
   paramStruct.clwCoeffPath   = 'not.set'

   ; NOTES: Eric S. Maddy 
   ; COAT path on orbit machines /data/data086/emaddy/dat/coat3/
   ; --- files are on /data/data086/emaddy/dat/coat3/scripts/lists/

   ;---ISS_RapidScatScene files
   paramStruct.AMVSceneListFile = 'scripts/lists/obs_wnd_20141215_iss_rapidscat.list'
   
   paramStruct.sensorName    = 'ISS_RapidScat'
   paramStruct.sensorID      = 'ISS_RapidScat'
   paramStruct.date          = '20141215'

   ;---for ECMWF 
   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_ECM_20141215_iss_rapidscat.list'

   amvOptions = {figPath: './images/iss_rapidscat/', $
                 nwpStr: 'ECMWF', $
                 TimeStr: 'XX', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 0, $
                 psorpng: 0, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                sensor: 'ISS RapidScat'}


   ;---whatever floats your boat
   amvOptions.figStr = paramStruct.date + '_' + $
                       amvOptions.TimeStr + '_' + amvOptions.nwpStr ;  + 'qcmbl'
   
   ;---copy amvOptions structure to paramStruct structure  
   paramStruct.amvOptions    = amvOptions 


END
