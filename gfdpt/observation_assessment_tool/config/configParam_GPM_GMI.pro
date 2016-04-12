;---------------------------------------------------------------------------------
; Name:  configParam_GPM_GMI.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for GPM_GMI
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO configParam_GPM_GMI, paramStruct
   ; Set map range to plot
   paramStruct.MIN_LAT = -90
   paramStruct.MAX_LAT = 90
   paramStruct.MIN_LON = -180
   paramStruct.MAX_LON = 180

   ; Set Max profile number and channel number
   paramStruct.MAX_FOV = 700000L
   paramStruct.MAX_CHAN = 13L 

   ; Radiance files (obs + sim)
   ; ECMWF files 
   paramStruct.radListFile2 = 'fwd_fix_em.gpm.ecm'
   ; Scene file
   paramStruct.sceneListFile = 'scene.gpm.ecm'

   paramStruct.radListFile1 = 'meas.gpm'
   paramStruct.radListFile2 = 'fwd_fix_em.gpm'
   ; Scene file
   paramStruct.sceneListFile = 'scene.gpm'

   paramStruct.radListFile1 = 'measured.list'
   paramStruct.radListFile2 = 'simulated.list'
   ; Scene file
   paramStruct.sceneListFile = 'observation.list'

   paramStruct.radListFile1 = 'meas.gpm.new'
   paramStruct.radListFile2 = 'sim.gpm.new'
   ; Scene file
   paramStruct.sceneListFile = 'scene.gpm.new'
 
   p = '/disks/data086/emaddy/dat/coat3/scripts/lists/'
   paramStruct.radListFile1  = p + 'obs_rad_20140905_gpm_gmi.list'
   paramStruct.radListFile2  = p + 'FWD_NWP_GFS_20140905_gpm_gmi.list'
   paramStruct.sceneListFile = p + 'NWP_GFS_20140905_gpm_gmi.list'
   paramStruct.sceneListFile = 'gpm.140905.scened'

   ;---test of CRTM coefficients
   paramStruct.radListFile1  = p + 'FWD_NWP_GFS_20140810_gpm_gmi.list2'
   paramStruct.radListFile2  = p + 'FWD_NWP_GFS_20140810_gpm_gmi.list'
   paramStruct.sceneListFile = p + 'NWP_GFS_20140810_gpm_gmi.list'
   paramStruct.sceneListFile = 'gpm.140810.scened'

   ;----------------------------------------------
   ;---files for Erin
   paramStruct.radListFile1  = p + 'meas.gpm.list'
   ;---with crtm emis 
   paramStruct.radListFile2  = p + 'fwd_gpm.list.5'
   paramStruct.radListFile2  = p + 'fwd_gpm.list'
   paramStruct.sceneListFile = p + 'nwp_gpm.list'
   paramStruct.sceneListFile = p + 'gpm.140804.list'

   ;----------------------------------------------

   ; Bias and standard deviation files
   paramStruct.biasListFile = 'nt_tools/bias.list'
   paramStruct.stddevListFile = '_tools/stddev.list'

   paramStruct.clwCoeffPath = './data/regr_coeffs/'
   paramStruct.chanNumArr =   $
       ['1','2','3','4','5','6','7','8','9','10',   $
       '11','12','13']

   paramStruct.chanInfoArr  =  $
       ['10.65V','10.65H','18.7V','18.7H','23.8V',$
        '36.5V','36.5H','89.0V','89.0H','165.5V','165.5H',$
        '183.31V','183.31H']
   paramStruct.minBT_Values =  $
       [ 160, 130, 170, 170, 200, 200, 200, 210, 200, 190,  $
       190, 200, 200, 230, 240, 150, 150, 170, 200, 170,           $
       170, 170, 170, 170 ]
   paramStruct.minBT_Values =  FLTARR(13) + 60.
   paramStruct.maxBT_Values =  $
       [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
       250, 240, 250, 260, 280, 300, 320, 310, 300, 300,           $
       300, 280, 280, 280 ]
   paramStruct.maxDTb_Values =  $
       [ 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20, 20, 20, 20, 20, 20, 20, $
         20, 20, 20, 20 ]
   paramStruct.minBT_Values =  FLTARR(13) + 20.
   paramStruct.maxDTb_Values =  FLTARR(13) + 20.
   paramStruct.maxBT_Values =  FLTARR(13) + 320.
   
   paramStruct.minBT_Values[0:5] = [140., 70., 170., 90., 180., 190.]
   paramStruct.maxBT_Values[0:5] = [220., 140., 230., 170., 270., 240.]
   
   paramStruct.sensorName = 'GPM_GMI'
   paramStruct.sensorID= 'gmi'
   paramStruct.chPlotArr = INDGEN(13)
   paramStruct.date = '2014-06-23'
   ; Image output dir
   paramStruct.imageDir     = './images/GPM/'
   paramStruct.imageDir     = './images/GPMgfs/'
   paramStruct.imageDir     = './images/GPMgfsc/'

   paramStruct.date = '2014-09-05'
   paramStruct.imageDir     = './images/GPMgfs_new/'
   paramStruct.imageDir     = './images/GPMgfs_new2/'
   paramStruct.imageDir     = './images/GPMgfs_new3/'

   paramStruct.date = '2014-08-04'
   ;---test of CRTM coefficients used in GSI and COAT
   paramStruct.imageDir     = './images/GPMgfs_crtm/'
   paramStruct.imageDir     = './images/GPMgfs_gsi00z_crtmemis/'
   paramStruct.imageDir     = './images/GPMgfs_gsi00z/'

   paramStruct.date = '2014-08-10'
   ;---test of CRTM coefficients used in GSI and COAT
   paramStruct.imageDir     = './images/GPMgfs_crtm/'
   paramStruct.imageDir     = './images/GPMgfs_emisreg/'

   ;---files with RFI in them 
   paramStruct.radListFile1  = p + 'obs_rad_20140810_gpm_gmi.list'

 
   ; -----------------------------------------------------------------
   ; THE following sets were used for dars and GSI comparisons 
   ; processed 20150309
   ; 
   ; ------------------------------------------------------------------
   ;---gfs
   paramStruct.radListFile2  = p + 'FWD_NWP_GFS_20140810_gpm_gmi.list'
   paramStruct.sceneListFile = p + 'NWP_GFS_20140810_gpm_gmi.list'
   paramStruct.sceneListFile = 'gpm.140810.scened'
   paramStruct.imageDir     = './images/GPMgfs_regress/'
   ; ESM :: 150309 -- latest and greatest for GFS
   paramStruct.imageDir     = './images/GPMgfs_emisnew/'

   ;---ecmwf
   paramStruct.radListFile2  = p + 'FWD_NWP_ECMWF_20140810_gpm_gmi.list'
   paramStruct.sceneListFile = p + 'NWP_ECMWF_20140810_gpm_gmi.list'
   paramStruct.sceneListFile = 'gpm.140810.scened.ecm'
   paramStruct.imageDir      = './images/GPMecm_regress/'
   paramStruct.imageDir      = './images/GPMecm_emis/'
   ; ESM :: 150309 -- latest and greatest for ECMWF
   paramStruct.imageDir      = './images/GPMecm_emisnew/'

   ; ESM :: 150325 -- latest and greatest for ECMWF (fastem)
   paramStruct.imageDir      = './images/GPMecm_emisfem/'

   ; ESM :: 150401 -- latest and greatest for ECMWF (fastem - single)
   paramStruct.imageDir      = './images/GPMecm_emisfems/'


   paramStruct.histogramOptions.trimOption  = INTARR(13) + 2
   paramStruct.histogramOptions.trimValue   = FLTARR(13) + 1.
   paramStruct.histogramOptions.useforclear = [INTARR(9) + 1,LONARR(4)]
   ; turn off clear filtering using histogram
   ; useforclear = [0, 0, 0 ..., 0]
   paramStruct.histogramOptions.useforclear = [LONARR(13)]

   ;---options for GPM clear filtering using window or surface
   ;   channels regression analytic emissivity by inversion of RTE and CRTM
    paramStruct.emisOptions.useForClear[0:12]  = 0   ; turn off filter

   paramStruct.emisOptions.maxValue[0] = 0.45   ; 50GHz
   paramStruct.emisOptions.maxValue[2] = 0.68  ; 18.7V
   paramStruct.emisOptions.maxValue[3] = 0.40  ; 18.7H
   paramStruct.emisOptions.maxValue[4] = 0.71  ; 23.8V
   ;; old values (internal usage and fits - ESM)
   ;; paramStruct.emisOptions.maxValue[2] = 0.61  18.7V
   ;; paramStruct.emisOptions.maxValue[3] = 0.35  18.7H
   ;; paramStruct.emisOptions.maxValue[4] = 0.71  23.8V

   ;---turn on filtering for 18V, 18H and 23V channels (4,5,6)
   paramStruct.emisOptions.useForClear[2] = 1  ; 18.7V GHz
   paramStruct.emisOptions.useForClear[3] = 1  ; 18.7H GHz
   paramStruct.emisOptions.useForClear[4] = 1  ; 23.8V GHz

   paramStruct.emisOptions.useForClear[0:12]  = 0   ; turn off filter

END
