;---------------------------------------------------------------------------------
; Name:  configParam_NOAA_18_AMSUA_MHS.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for NOAA_18_AMSUA_MHS
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;          Oct 20, 2014, MoharC, Modifications for AMSU_MHS
;
;---------------------------------------------------------------------------------
PRO configParam_NOAA_18_AMSUA_MHS, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 30000L
   paramStruct.MAX_CHAN = 40L 

   ; Radiance files (obs + sim)
   ;;paramStruct.radListFile1 = '/data/home001/dxu/ssmis_assessment_tools/meas.list'
   paramStruct.radListFile1 = 'measured.list'
   ;;paramStruct.radListFile2 = '/data/home001/dxu/ssmis_assessment_tools/fwd_fix_em.list'
   paramStruct.radListFile2 = 'simulated.list'
   ; Scene file
    paramStruct.sceneListFile = 'scene.list'
   ;;paramStruct.sceneListFile = '/data/home001/dxu/ssmis_assessment_tools/scene.list'
   ; Bias and standard deviation files
   paramStruct.biasListFile = '/data/home001/dxu/ssmis_assessment_tools/bias.list'
   paramStruct.stddevListFile = '/data/home001/dxu/ssmis_assessment_tools/stddev.list'
   paramStruct.clwCoeffPath = '/data/home001/dxu/data_assessment_tool/data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13','14','15','16','17','18','19','20']
   paramStruct.chanInfoArr  =  $
       ['23.8', '31.4', '50.30', '52.8', '53.596', '54.40',$
       '54.94', '55.5', '57.29', '57.29', '57.29', '57.29', $
       '57.29', '57.29', '89.0', '89.0', '157.0', '183.3',  $
       '183.3', '190.3' ]

   paramStruct.minBT_Values =  FLTARR(20) + 20.
   paramStruct.maxBT_Values =  FLTARR(20) + 350.
   paramStruct.maxDTb_Values =  $
       [ 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20, 20, 20, 20, 20, 20, 20]
   paramStruct.imagedir = './images/n18/'
   paramStruct.sensorName = 'NOAA_18_AMSUA_MHS'
   paramStruct.sensorID = 'noaa18'
   paramStruct.chPlotArr = INDGEN(20)
   paramStruct.date = '2012-10-27'

   ;---turn off histogram trimming for now
   paramStruct.histogramOptions.trimOption = INTARR(20)  
   paramStruct.histogramOptions.trimValue  = FLTARR(20)   + 1.
   paramStruct.histogramOptions.useforclear = INTARR(20)
END
