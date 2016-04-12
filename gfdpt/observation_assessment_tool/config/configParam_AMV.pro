;---------------------------------------------------------------------------------
; Name:  configParam_AMV.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for AMV
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Eric S. Maddy (RTI) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: May 21, 2014, EMaddy, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_AMV, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV  = 100000L
   paramStruct.MAX_CHAN = 2L 

   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = 'not.set'
   paramStruct.radListFile2 = 'not.set'
   ; Bias and standard deviation files (AMV not needed)
   paramStruct.biasListFile   = 'not.set'
   paramStruct.stddevListFile = 'not.set'
   ; cloud-liquid water regressions path (AMV not needed)
   paramStruct.clwCoeffPath   = 'not.set'

   ; AMVScene files
   paramStruct.AMVsceneListFile = 'amvscene12z.list'
   paramStruct.AMVsceneListFile = 'amvscene00z.list'
   paramStruct.sensorName    = 'AMV'
   paramStruct.sensorID      = 'AMV'
   paramStruct.date          = '20140401_05'

   ;---for ECMWF 
   paramStruct.NWPsceneListFile = 'nwpscene00z.list.ecm'
   amvOptions = {figPath: './images/amv/', $
                 nwpStr: 'ECMWF', $
                 TimeStr: '00', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 1, $
                 psorpng: 1, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'SATWND BUFR'}

   ; AVHRR global AMVs
   paramStruct.AMVsceneListFile = 'scripts/lists/obs_wnd_20150203_avhrrglamv.list'
   
   paramStruct.sensorName    = 'AVHRR'
   paramStruct.sensorID      = 'AMV'
   paramStruct.date          = '20150203'

   ;---for GFS6hr
   paramStruct.NWPsceneListFile = 'nwpscene12z.list'
   paramStruct.NWPsceneListFile = 'nwpscene00z.list'

   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_GFS_20150203_avhrrglamv.list'
   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_ECM_20150203_avhrrglamv.list'
   ;---options for AMV winds 
   amvOptions = {figPath: './images/avhrrgl_amv/', $
                 nwpStr: 'GFS6hr', $
                 TimeStr: '00-23', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 0, $
                 psorpng: 1, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'AVHRR Global Winds'}

   ;---options for AMV winds 
   amvOptions = {figPath: './images/avhrrgl_amv/', $
                 nwpStr: 'ECMWFANL', $
                 TimeStr: '00-18', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 0, $
                 psorpng: 1, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'AVHRR Global Winds'}

   ;---for NRL super obs 
   date = '20141023'
   date = '20140902'
   
   paramStruct.AMVsceneListFile = 'scripts/lists/obs_wnd_' + date + '_nrl_satwnd.list'
   
   paramStruct.sensorName    = 'SATWND'
   paramStruct.sensorID      = 'AMV'
   paramStruct.date          = date

   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_' + date + '_nrl_satwnd.list'
   ;---options for AMV winds 

   amvOptions = {figPath: './images/nrl_satwndgfs/', $
                 nwpStr: 'GFS6hr', $
                 TimeStr: '00-18', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 1, $
                 psorpng: 1, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'SATWND NRL'}

   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_ECM_' + date + '_nrl_satwnd.list'
   ;---options for AMV winds 
   amvOptions = {figPath: './images/nrl_satwndecm/', $
                 nwpStr: 'ECMWFANL', $
                 TimeStr: '00-18', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 0, $
                 psorpng: 0, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'SATWND NRL'}

   ;---for comparison to NRL
   date = '20141023'
   date = '20140902'
   paramStruct.AMVsceneListFile = 'scripts/lists/obs_wnd_' + date + '_gdas_satwnd.list'
   
   paramStruct.sensorName    = 'SATWND'
   paramStruct.sensorID      = 'AMV'
   paramStruct.date          = date

   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_GFS_' + date + '_gdas_satwnd.list'
   ;---options for AMV winds 

   amvOptions = {figPath: './images/gdas_satwndgfs/', $
                 nwpStr: 'GFS6hr', $
                 TimeStr: '00-18', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 1, $
                 psorpng: 1, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'SATWND BUFR'}

   paramStruct.NWPsceneListFile = 'scripts/lists/NWPWND_ECM_' + date + '_gdas_satwnd.list'
   ;---options for AMV winds 
   amvOptions = {figPath: './images/gdas_satwndecm/', $
                 nwpStr: 'ECMWFANL', $
                 TimeStr: '00-18', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 0, $
                 psorpng: 0, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0, $
                 sensor: 'SATWND BUFR'}


   ;---whatever floats your boat
   amvOptions.figStr = paramStruct.date + '_' + $
                       amvOptions.TimeStr + '_' + amvOptions.nwpStr ;  + 'qcmbl'
   
   ;---copy amvOptions structure to paramStruct structure  
   paramStruct.amvOptions    = amvOptions 



END
