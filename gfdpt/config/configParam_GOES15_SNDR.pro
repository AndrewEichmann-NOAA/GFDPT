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
PRO configParam_GOES15_SNDR, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -20.
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 80000L
   paramStruct.MAX_CHAN = 18L 

   ; Radiance files (obs + sim)
   ; ls -1 -d data/TestbedData/fmsdr_sim/goes13_sndrd1/2014-10-05/FWD_NWP_GFS* > fwd.g13
   ; ls -1 -d data/TestbedData/fmsdr/goes13_sndrd1/2014-10-05/g13* > meas.g13
   ; ls -1 -d data/TestbedData/edr/goes13_sndrd1/2014-10-05/NWP_GFS* > scene.g13

   ; ls -1 -d data/TestbedData/fmsdr_sim/goes13_sndrd?/2014-10-05/FWD_NWP_GFS* > fwd.g13.all
   ; ls -1 -d data/TestbedData/fmsdr/goes13_sndrd?/2014-10-05/g13* > meas.g13.all
   ; ls -1 -d data/TestbedData/edr/goes13_sndrd?/2014-10-05/NWP_GFS* > scene.g13.all
   ; Bias and standard deviation files
   paramStruct.biasListFile = 'bias.list'
   paramStruct.stddevListFile = 'stddev.list'
   paramStruct.clwCoeffPath = 'data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13','14','15','16','17','18']
   paramStruct.chanInfoArr  =  $
          ['14.71', '14.37', '14.06','13.64','13.37','12.66',$
          '12.02', '11.03', '9.71', '7.43', '7.02','6.51',$
          '4.57', '4.52', '4.45', '4.13', '3.98', '3.74']
   paramStruct.freq_unit = '!9m!Xm'  ; should be postscript font
   paramStruct.minBT_Values =  FLTARR(18) + 30.
   paramStruct.maxBT_Values =  FLTARR(18) + 340.
   paramStruct.maxDTb_Values = FLTARR(18) + 20.
   paramStruct.chPlotArr = INDGEN(18)
   paramStruct.date = '2014-10-05'
   ; single detectors GOES SNDR 
   paramStruct.radListFile1 = 'goeslist/meas.g15d1'
   paramStruct.radListFile2 = 'goeslist/fwd.g15d1'
   paramStruct.sceneListFile = 'goeslist/scene.g15d1'
   paramStruct.sensorName = 'GOES13_SNDRD1'
   paramStruct.sensorID = 'GOES13D1'
   paramStruct.imageDir     = './images/g15_sndrd1/'

   ; all detectors GOES SNDR 
   paramStruct.radListFile1 = 'goeslist/meas.g15dall'
   paramStruct.radListFile2 = 'goeslist/fwd.g15dall'
   paramStruct.sceneListFile = 'goeslist/scene.g15dall'
   paramStruct.sensorName = 'G15_SNDR'
   paramStruct.sensorID = 'G15'
   paramStruct.imageDir     = './images/g15_sndrdall2/'
   paramStruct.histogramOptions.trimOption  = INTARR(18) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(18) + 1.
   ; turn off clear filtering using histogram
   ; useforclear = [0, 0, 0 ..., 0]
   paramStruct.histogramOptions.useforclear = [LONARR(18)]

END
