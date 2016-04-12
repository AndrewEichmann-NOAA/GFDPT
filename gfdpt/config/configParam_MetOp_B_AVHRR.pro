;---------------------------------------------------------------------------------
; Name:  configParam_MetOp_B_AVHRR.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for MetOp_B_AVHRR
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Eric Maddy (RTI) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: Oct 25, 2015, ESM, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_MetOp_B_AVHRR, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90.
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 500000L
   paramStruct.MAX_CHAN = 3L 

   ; Bias and standard deviation files
   paramStruct.biasListFile = 'bias.list'
   paramStruct.stddevListFile = 'stddev.list'
   paramStruct.clwCoeffPath = 'data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['3b','4','5']
   paramStruct.chanInfoArr  =  $
      ['3.74', '10.8', '12.0']

   paramStruct.freq_unit = '!9m!Xm'  ; should be postscript font
   nchan = 3
   paramStruct.minBT_Values =  FLTARR(nchan) + 200.
   paramStruct.maxBT_Values =  FLTARR(nchan) + 340.
   paramStruct.maxDTb_Values = FLTARR(nchan) + 20.
   paramStruct.chPlotArr = INDGEN(nchan)

   paramStruct.date = '2015-10-20'

   timerun = '00z'
   paramStruct.radListFile1 = $
      'scripts/lists/obs_rad_20151020_metopb_avhrr.list.' + timerun 
   paramStruct.radListFile2 = $
      'scripts/lists/FWD_NWP_ECMWF_20151020_metopb_avhrr.list.' + timerun 
   paramStruct.sceneListFile = $
      'scripts/lists/NWP_ECMWF_SD_20151020_metopb_avhrr.list.' + timerun 
   paramStruct.imageDir     = './images/metopb_avhrr' + timerun + '/'

   paramStruct.sensorName = 'MetOpB_AVHRR'
   paramStruct.sensorID = 'AVHRR'

   paramStruct.histogramOptions.trimOption  = INTARR(nchan) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(nchan) + 1.
   ; turn off clear filtering using histogram
   ; useforclear = [0, 0, 0 ..., 0]
   paramStruct.histogramOptions.useforclear = [LONARR(nchan)]

END
