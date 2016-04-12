;---------------------------------------------------------------------------------
; Name:  configParam_GOES13_IMGR.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for GOES13 IMAGER
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Eric S. Maddy (RTI) @ JCSDA,
;         Eric.Maddy@noaa.gov
; Version: Nov. 2014, E. S. Maddy
;
;
;---------------------------------------------------------------------------------
PRO configParam_GOES13_IMGR, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90.
   paramStruct.MAX_LAT = 90.
   paramStruct.MIN_LON = -180.
   paramStruct.MAX_LON = 180.

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 80000L
   paramStruct.MAX_CHAN = 4L 

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
       ['1','2','3','4']
   paramStruct.chanInfoArr  =  $
          ['3.9', '6.75', '10.7', '12.0']
   paramStruct.freq_unit = '!9m!Xm'  ; should be postscript font
   paramStruct.minBT_Values =  FLTARR(4) + 30.
   paramStruct.maxBT_Values =  FLTARR(4) + 340.
   paramStruct.maxDTb_Values = FLTARR(4) + 20.
   paramStruct.chPlotArr = INDGEN(4)
   paramStruct.date = '2014-10-05'

   ; all detectors GOES SNDR 
   paramStruct.radListFile1 = 'goeslist/meas.g13idall'
   paramStruct.radListFile2 = 'goeslist/fwd.g13idall'
   paramStruct.sceneListFile = 'goeslist/scene.g13idall'

   paramStruct.radlistfile1='/disks/data086/emaddy/dat/observation_assessment_tool//scripts/lists/obs_rad_20141005_goes13_imgr.list'
   paramStruct.scenelistfile='/disks/data086/emaddy/dat/observation_assessment_tool//scripts/lists/NWP_ECMWF_20141005_goes13_imgr.list'
   paramStruct.radlistfile2='/disks/data086/emaddy/dat/observation_assessment_tool//scripts/lists/FWD_NWP_ECMWF_20141005_goes13_imgr.list'

   paramStruct.sensorName = 'G13_IMGR'
   paramStruct.sensorID = 'G13'
   paramStruct.imageDir     = './images/g13_imgrdall/'
   paramStruct.histogramOptions.trimOption  = INTARR(4) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(4) + 1.
   ; turn off clear filtering using histogram
   ; useforclear = [0, 0, 0 ..., 0]
   paramStruct.histogramOptions.useforclear = [LONARR(4)]

END
