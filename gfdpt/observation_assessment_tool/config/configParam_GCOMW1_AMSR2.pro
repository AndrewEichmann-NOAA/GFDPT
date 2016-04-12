;---------------------------------------------------------------------------------
; Name:  configParam_GCOMW1_AMSR2.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for GCOMW1_AMSR2
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;          Jul 1, 2014, ESMaddy, Updates for real data
;
;---------------------------------------------------------------------------------
PRO configParam_GCOMW1_AMSR2, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 500000L
   paramStruct.MAX_CHAN = 14L 

   paramStruct.radListFile2 = 'meas.gcom'
   ; Radiance files (obs + sim)
   paramStruct.radListFile1 = 'meas.nesdis.gcom'
   paramStruct.radListFile2 = 'fwd.gcom'
   paramStruct.sceneListFile = 'scene.gcom'
   ; updated gfs for stratospheric levels
   paramStruct.radListFile2 = 'fwd_fix_em.gcom.gfs'
   ; Scene file
   paramStruct.sceneListFile = 'scene.gcom.gfs'

   paramStruct.radListFile1 = 'meas.nesdis0723.gcom'

   paramStruct.radListFile1 = 'meas.jaxa0723.gcom'
   paramStruct.radListFile2 = 'fwd.jaxa0723.gcom'
   paramStruct.sceneListFile = 'scened.jaxa0723.gcom'

   ; Bias and standard deviation files
   paramStruct.biasListFile = '/data/ejones/tools/obs_assessment/obs_assessment/bias.list'
   paramStruct.stddevListFile = '/data/ejones/tools/obs_assessment/obs_assessment/stddev.list'
   paramStruct.clwCoeffPath = 'data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13','14']
   paramStruct.chanInfoArr  =  $
       ['6.925','6.925','7.30','7.30','10.65','10.65','18.7','18.7','23.8','23.8','36.5','36.5','89.0','89.0']
   paramStruct.minBT_Values =  $
       [ 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40 ]
   paramStruct.maxBT_Values =  $
       [ 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350 ]
   paramStruct.maxDTb_Values =  $
       [ 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 ]
   paramStruct.sensorName = 'GCOMW1_AMSR2'
   paramStruct.sensorID= 'amsr2'
   paramStruct.chPlotArr = INDGEN(14)
   ; paramStruct.chPlotArr = [1,2]
   paramStruct.date = '2013-07-22'
   paramStruct.date = '2013-07-23'

   ;; paramStruct.imagedir = './images/gcomw1/'
   ;; paramStruct.histogramOptions.trimOption = INTARR(14) + 1
   ;; paramStruct.histogramOptions.trimValue = FLTARR(14) + 1.
   ;; paramStruct.histogramOptions.useforclear = INTARR(14) + 1

   paramStruct.imagedir = './images/gcomw1_diff/'
   ; updated gfs for stratospheric levels

   paramStruct.imagedir = './images/gcomw1_nesdis_gfs2test/'
   paramStruct.histogramOptions.trimOption = INTARR(14)  
   paramStruct.histogramOptions.trimValue  = FLTARR(14)   + 1.
   paramStruct.histogramOptions.useforclear = [0, 0, 0, 0, 0, 0, 0, 0, $
                                               0, 0, 0, 0, 0, 0]

   paramStruct.imagedir = './images/gcomw1_nesdis0723/'
   paramStruct.imagedir = './images/gcomw1_jaxa0723/'
   ; use 18.7, 36.5 and 89. GHz channels for clear test
   paramStruct.histogramOptions.trimOption = INTARR(14)  + 2.
   paramStruct.histogramOptions.useforclear = [0, 0, 0, 0, 0, 0, 1, 1, $
                                               0, 0, 1, 1, 1, 1]

   paramStruct.histogramOptions.trimOption = INTARR(14)  + 10.
   paramStruct.histogramOptions.useforclear = [0, 0, 0, 0, 0, 0, 1, 1, $
                                               0, 0, 1, 1, 1, 1]


END
