;---------------------------------------------------------------------------------
; Name:  configParam_F18_SSMIS.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for F18_SSMIS
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_F18_SSMIS, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 70000L
   paramStruct.MAX_CHAN = 24L 

   ; -----------------------------------------
   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = './meas.mirs'
   paramStruct.radListFile2 = './fwd.mirs'
   ; Scene file
   paramStruct.sceneListFile = './scene.mirs'
   ; Image output dir
   paramStruct.imageDir     = './images/f18/'
   ; -----------------------------------------

   ; -----------------------------------------
   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = './meas.list'
;   paramStruct.radListFile2 = './fwd_fix_em.list'
   ; Scene file
   paramStruct.radListFile2 = './fwd_fix_em.list.gfs'
   paramStruct.sceneListFile = './scene.list.gfs'
   p = '/disks/data086/emaddy/dat/coat3/scripts/lists/'
   paramStruct.radlistfile1  = p + 'obs_rad_20130120_f18_ssmis.list'
   paramStruct.radListFile2  = p + 'FWD_NWP_GFS_20130120_f18_ssmis.list'
   ;---file with old COAT emissivity 
   paramStruct.scenelistfile = p + 'NWP_GFS_20130120_f18_ssmis.list'
   paramStruct.sceneListFile = 'f18.130120.scene'

   ; new files with emissivity in the dump
   paramStruct.radListFile2  = p + 'FWD_NWP_ECMWF_20130120_f18_ssmis.list'
   paramStruct.sceneListFile = 'f18.130120.scene.ecm'

   paramStruct.radListFile2  = p + 'FWD_NWP_GFS_20130120_f18_ssmis.list'
   paramStruct.sceneListFile = 'f18.130120.scene'

;  paramStruct.sceneListFile = './scene.list'
   ; Image output dir
   paramStruct.imageDir     = './images/f18dxu/'
   paramStruct.imageDir     = './images/f18dxugfs/'
   paramStruct.imageDir     = './images/f18dxugfs_crtmemis/'
   paramStruct.imageDir     = './images/f18dxugfs_coatemis/'
   paramStruct.imageDir     = './images/f18dxuecm/'
;   paramStruct.imageDir     = './images/f18dxuecm_emisreg/'

   paramStruct.imageDir     = './images/f18dxuecm_regress/'

   paramStruct.imageDir     = './images/f18dxugfs_emis/'

;   paramStruct.imageDir     = './images/f18dxuecm_emisreg/'
  ; -----------------------------------------

   ; Bias and standard deviation files
   paramStruct.biasListFile = './bias.list'
   paramStruct.stddevListFile = './stddev.list'
   paramStruct.biasFile     = './data/bias_coeffs/biasCorrec_f18.dat'
   paramStruct.clwCoeffPath = './data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13','14','15','16','17','18','19','20',      $
       '21','22', '23', '24']
   paramStruct.chanInfoArr  =  $
       ['50.300', '52.800', '53.596', '54.400', '55.500', '57.290',$
       '59.400', '150.000', '183.310', '183.310', '183.310', '19.350', $
       '19.350', '22.235', '37.000', '37.000', '91.655', '91.655',     $
       '63.283', '60.793', '60.793', '60.793', '60.793', '60.793' ]
   paramStruct.minBT_Values =  $
       [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
       190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
       170, 170, 170, 170 ]
   paramStruct.maxBT_Values =  $
       [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
       250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
       300, 280, 280, 280 ]
   paramStruct.minBT_Values =  FLTARR(24) + 60.
   paramStruct.maxBT_Values =  FLTARR(24) + 320.

   paramStruct.maxDTb_Values =  $
       [ 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20 ]
   paramStruct.maxDTb_Values =  $
      FLTARR(24) + 20.
   paramStruct.sensorName = 'F18_SSMIS'
   paramStruct.sensorID = 'f18'
   paramStruct.chPlotArr = INDGEN(24)
   ;paramStruct.chPlotArr = [0,1,2,3]
   paramStruct.date = '2013-01-20'

   paramStruct.histogramOptions.trimOption  = INTARR(24) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(24) + 1.5
;   paramStruct.histogramOptions.useforclear = [INDGEN(7), 0, INDGEN(10)]

   paramStruct.histogramOptions.useforclear = [INTARR(11), INTARR(5)+1, INTARR(8)]
   paramStruct.histogramOptions.useforclear = [INTARR(24)]

   ;---options for F18 SSMI/S clear filtering using window or surface
   ;   channels regression analytic emissivity by inversion of RTE and CRTM
    paramStruct.emisOptions.useForClear[0:23]  = 0   ; turn off filter

   paramStruct.emisOptions.maxValue[0]  = 0.45   ; 50GHz
   paramStruct.emisOptions.maxValue[11] = 0.31  ; 19.35GHz
   paramStruct.emisOptions.maxValue[12] = 0.61  ; 19.35GHz
   paramStruct.emisOptions.maxValue[13] = 0.61  ; 22GHz
   paramStruct.emisOptions.maxValue[14] = 0.38  ; 22GHz
   paramStruct.emisOptions.maxValue[15] = 0.70  ; 37GHz

   paramStruct.emisOptions.useForClear[0]  = 1   ; 50GHz
   paramStruct.emisOptions.useForClear[11] = 1  ; 19.35GHz
   paramStruct.emisOptions.useForClear[12] = 1  ; 19.35GHz
   paramStruct.emisOptions.useForClear[13] = 1  ; 22GHz
   paramStruct.emisOptions.useForClear[13] = 1  ; 22GHz
   paramStruct.emisOptions.useForClear[14] = 1  ; 37GHz
   paramStruct.emisOptions.useForClear[15] = 1  ; 37GHz
;   paramStruct.emisdiffOptions.useForClear[16] = 1  ; 91GHz
;   paramStruct.emisdiffOptions.useForClear[17] = 1  ; 91GHz

;   paramStruct.emisOptions.useForClear[0:23]  = 0   ; turn off filter

END
