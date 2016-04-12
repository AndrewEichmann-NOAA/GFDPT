;---------------------------------------------------------------------------------
; Name:  configParam_MT_SAPHIR.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for MT_SAPHIR
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Eric S. Maddy
; Version: Nov 21, 2014, ES Maddy 
;
;
;---------------------------------------------------------------------------------
PRO configParam_MT_SAPHIR, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 1.6E6
   paramStruct.MAX_CHAN = 6L 

   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = '/disks/data086/emaddy/dat/coat3//scripts/lists/obs_rad_20140920_mtsa_saphir.list'

   ; GFS FWD 
   paramStruct.radListFile2 = '/disks/data086/emaddy/dat/coat3//scripts/lists/FWD_NWP_GFS_20140920_mtsa_saphir.list'
   ; Scene file -- GFS
   paramStruct.sceneListFile = '/disks/data086/emaddy/dat/coat3//scripts/lists/SDNWP_GFS_20140920_mtsa_saphir.list'
   paramStruct.imageDir     = './images/mtsa_saphir/'

   ; ECM FWD 
   paramStruct.radListFile2 = '/disks/data086/emaddy/dat/coat3//scripts/lists/FWD_NWP_ECMWF_20140920_mtsa_saphir.list'
   ; Scene file -- ECM
   paramStruct.sceneListFile = '/disks/data086/emaddy/dat/coat3//scripts/lists/SDNWP_ECMWF_20140920_mtsa_saphir.list'
   paramStruct.imageDir     = './images/mtsa_saphir_ecm/'

   ; Bias and standard deviation files
   paramStruct.biasListFile = 'bias.list'
   paramStruct.stddevListFile = 'stddev.list'
   paramStruct.clwCoeffPath = '/data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['1','2','3','4','5','6']
   paramStruct.chanInfoArr  =  $
       ['183.', '183.', '183.', '183.', '183.', '183.']
   paramStruct.minBT_Values =  FLTARR(6) + 40.
   paramStruct.maxBT_Values =  FLTARR(6) + 350.
   paramStruct.maxDTb_Values =  $
       [ 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20 ]
   paramStruct.sensorName = 'MT_SAPHIR'
   paramStruct.sensorID= 'saphir'
   paramStruct.chPlotArr = INDGEN(6)
   paramStruct.date = '2014-09-20'
   paramStruct.histogramOptions.trimOption  = INTARR(6) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(6) + 1.
   ; turn off clear filtering using histogram
   ; useforclear = [0, 0, 0 ..., 0]
   paramStruct.histogramOptions.useforclear = [LONARR(6)]

END
