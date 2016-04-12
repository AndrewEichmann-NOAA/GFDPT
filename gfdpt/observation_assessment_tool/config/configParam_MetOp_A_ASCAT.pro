;---------------------------------------------------------------------------------
; Name:  configParam_MetOp_A_ASCAT.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for MetOp_A_ASCAT
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Eric S. Maddy (RTI) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: May 21, 2014, EMaddy, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_MetOp_A_ASCAT, paramStruct
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
   ; Bias and standard deviation files (MetOp_A_ASCAT not needed)
   paramStruct.biasListFile   = 'not.set'
   paramStruct.stddevListFile = 'not.set'
   ; cloud-liquid water regressions path (MetOp_A_ASCAT not needed)
   paramStruct.clwCoeffPath   = 'not.set'

   ; MetOp_A_ASCATScene files
   paramStruct.MetOp_A_ASCATsceneListFile = 'amvscene12z.list'
   paramStruct.MetOp_A_ASCATsceneListFile = 'amvscene00z.list'
   
   paramStruct.sensorName    = 'MetOp_A_ASCAT'
   paramStruct.sensorID      = 'MetOp_A_ASCAT'
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
                 ecmwfqi: 0}

   ;---for GFS6hr
   paramStruct.NWPsceneListFile = 'nwpscene12z.list'
   paramStruct.NWPsceneListFile = 'nwpscene00z.list'

   ;---options for MetOp_A_ASCAT winds 
   amvOptions = {figPath: './images/ascat/', $
                 nwpStr: 'GFS6hr', $
                 TimeStr: '00', $
                 figStr: '', $
                 date: paramStruct.date, $
                 pngOrderflag: 1, $
                 psorpng: 1, $
                 qualityMark: 0, $
                 gsiblacklist: 0, $
                 highlevel: 0, $
                 lowlevel: 0, $
                 ecmwfqi: 0}


   ;---whatever floats your boat
   amvOptions.figStr = paramStruct.date + '_' + $
                       amvOptions.TimeStr + '_' + amvOptions.nwpStr ;  + 'qcmbl'
   
   ;---copy amvOptions structure to paramStruct structure  
   paramStruct.amvOptions    = amvOptions 



END
