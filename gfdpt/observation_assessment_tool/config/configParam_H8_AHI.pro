;---------------------------------------------------------------------------------
; Name:  configParam_NOAA_19_AMSUA_MHS.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for NOAA_19_AMSUA_MHS
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_H8_AHI, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90.
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 300000L
   paramStruct.MAX_CHAN = 10L 

   ; Bias and standard deviation files
   paramStruct.biasListFile = 'bias.list'
   paramStruct.stddevListFile = 'stddev.list'
   paramStruct.clwCoeffPath = 'data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['7','8','9','10','11','12','13','14','15','16']
   paramStruct.chanInfoArr  =  $
      ['3.885', '6.243', '6.941', '7.347', '8.593', '9.637', $
       '10.407', '11.240', '12.381', '13.281']

   paramStruct.freq_unit = '!9m!Xm'  ; should be postscript font
   paramStruct.minBT_Values =  FLTARR(10) + 200.
   paramStruct.maxBT_Values =  FLTARR(10) + 340.
   paramStruct.maxDTb_Values = FLTARR(10) + 20.
   paramStruct.chPlotArr = INDGEN(10)
   paramStruct.date = '2013-07-10'
   ; all detectors GOES SNDR 
   paramStruct.radListFile1 = 'scripts/lists/obs_rad_20140810_h8_ahi.list'
   paramStruct.radListFile2 = 'scripts/lists/FWD_NWP_GFS_20140810_h8_ahi.list'
   paramStruct.sceneListFile = 'scripts/lists/NWP_GFS_20140810_h8_ahi.list'

   paramStruct.radListFile1 = 'scripts/lists/obs_rad_20130710_h8_ahi.list'
   paramStruct.radListFile2 = 'scripts/lists/FWD_NWP_GFS_20130710_h8_ahi.list'
   paramStruct.sceneListFile = 'scripts/lists/NWP_GFS_20130710_h8_ahi.list'

   paramStruct.date = '2015-01-25'
   paramStruct.radListFile1 = 'scripts/lists/obs_rad_20150125_h8_ahi.list'
   paramStruct.radListFile2 = 'scripts/lists/FWD_NWP_GFS_20150125_h8_ahi.list'
   paramStruct.sceneListFile = 'scripts/lists/NWP_GFS_20150125_h8_ahi.list'
   paramStruct.imageDir     = './images/h8_ahi_sample/'
   paramStruct.imageDir     = './images/h8_ahi_samplecft/'

   paramStruct.date = '2015-01-25'
   paramStruct.radListFile1 = 'scripts/lists/obs_rad_20150125_h8_ahi.list'
   paramStruct.radListFile2 = 'scripts/lists/FWD_NWP_ECMWF_20150125_h8_ahi.list'
   paramStruct.sceneListFile = 'scripts/lists/NWP_ECMWF_20150125_h8_ahi.list'
   paramStruct.imageDir     = './images/h8_ahi_ecmsamplecft/'

   paramStruct.radListFile1 = '/data/data086/emaddy/himawari/files.list'
   paramStruct.radListFile2 = 'scripts/lists/FWD_NWP_ECMWF_20150125_h8_ahi.list'
   paramStruct.sceneListFile = 'scripts/lists/NWP_ECMWF_20150125_h8_ahi.list'
   paramStruct.imageDir     = './images/h8_ahi_ecmsamplecft_bt/'

   paramStruct.sensorName = 'H8_AHI'
   paramStruct.sensorID = 'AHI'

   paramStruct.histogramOptions.trimOption  = INTARR(10) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(10) + 1.
   ; turn off clear filtering using histogram
   ; useforclear = [0, 0, 0 ..., 0]
   paramStruct.histogramOptions.useforclear = [LONARR(10)]

END
