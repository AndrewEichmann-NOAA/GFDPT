;---------------------------------------------------------------------------------
; Name:  configSensorParam.pro
;
; Type:  IDL Program
;
; Description:
;   To config parameters for various sensors. 
;   It contains both shared config params and 
;   individual config params that are specific to each sensor.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
;         Eric S. Maddy (RTi) @ JCSDA
;         
; Version: Mar 5, 2014, DXu, Initial coding
;          Oct 15, 2014, ESM, increased size of arrays for
;          Hyperspectral IR
;
;---------------------------------------------------------------------------------
@importConfigParam.pro

PRO configSensorParam, sensorOption, paramStruct
   ;----------------------------------------------------
   ; Create a struct to hold all the config parameters
   ; Values need to overriden for each specific sensor. 
   ;----------------------------------------------------
   MAX_CHAN_NUM    = 8461L    ; Max channel number (IASI all)
   INT_FILL_Val    = -999
   FLOAT_FILL_Val  = -999.99
   STRING_FILL_Val = ''

   amvOptions = {        $
                figPath: './', $
                nwpStr: 'GFS', $
                TimeStr: '00-18', $
                figStr: '', $
                date: '', $
                pngOrderflag: 0, $
                psorpng: 0, $
                qualityMark: 0, $
                gsiblacklist: 0, $
                highlevel: 0, $
                lowlevel: 0, $
                ecmwfqi: 0, $
                sensor: 'SATWND BUFR'}

   histogramOptions = {   $
                      trimoption: MAKE_ARRAY(MAX_CHAN_NUM,/INT,$
                                             VALUE=0), $
                      trimvalue: MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT,$
                                             VALUE=FLOAT_FILL_Val), $
                      useforclear: MAKE_ARRAY(MAX_CHAN_NUM,/INT,$
                                             VALUE=INT_FILL_Val)}

   emisOptions = { $
                     maxValue: MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT,$
                                              VALUE=FLOAT_FILL_Val), $
                     useforclear:  MAKE_ARRAY(MAX_CHAN_NUM,/INT,$
                                             VALUE=INT_FILL_Val) }

   
   ;---These are defaults, any differences between values here and
   ;   user settings should be modified per instrument in 
   ;   config/configParam_SAT_SENSOR.pro
   paramStruct={          $
      INT_FILL    : INT_FILL_Val    ,$
      FLOAT_FILL  : FLOAT_FILL_Val  ,$
      STRING_FILL : STRING_FILL_Val ,$
      MIN_LAT : -90,      $
      MAX_LAT : 90,       $
      MIN_LON : -180,     $
      MAX_LON : 180,      $
      radListFile1 : '',  $
      radListFile2 : '',  $
      sceneListFile: '',  $
      biasListFile : '',  $
      stddevListFile : '',$
      biasFile : '',      $
      imageDir : '',      $
      histogramOptions: histogramOptions, $         
      emisOptions: emisOptions, $         
      MAX_FOV : 30000L,   $
      MAX_CHAN : MAX_CHAN_NUM , $ 
      sensorName: STRING_FILL_Val, $ 
      sensorID  : STRING_FILL_Val, $ 
      chanNumArr  : MAKE_ARRAY(MAX_CHAN_NUM,/STRING,    $
                               VALUE=STRING_FILL_Val),  $
      chanInfoArr   : MAKE_ARRAY(MAX_CHAN_NUM,/STRING,  $
                                 VALUE=STRING_FILL_Val),$
      minBT_Values  : MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT,   $
                                 VALUE=FLOAT_FILL_Val), $
      maxBT_Values  : MAKE_ARRAY(MAX_CHAN_NUM,/FLOAT,   $
                                 VALUE=FLOAT_FILL_Val), $
      maxDTb_Values : MAKE_ARRAY(MAX_CHAN_NUM,/INT,     $
                                 VALUE=INT_FILL_Val),   $
      chPlotArr     : MAKE_ARRAY(MAX_CHAN_NUM,/INT,     $
                                 VALUE=INT_FILL_Val),   $
      freq_unit     : 'GHz', $         
;      CLW_THRESHOLD_MIN : 0.00,  $  ;; RAIN , CLW Threshol = 0.05 And No CLW and Ice
      CLW_THRESHOLD_MIN : 0.05,  $  ;; These are defaults and should be modified in config/configParam_XXX_YYY.pro
      CLW_THRESHOLD_MAX : 0.3,   $
      RWP_THRESHOLD     : 0.05,  $  ;; RAIN , RWP Threshol = 0.2 And No CLW and Ice
      GWP_THRESHOLD     : 0.05,  $
      GWP_THRESHOLD_MIN : 0.05,  $
      GWP_THRESHOLD_MAX : 10.0,   $
      clwCoeffPath : '',  $
      date : '2013-01-20', $
      amvOptions : amvOptions, $
      amvscenelistfile : '',  $
      nwpscenelistfile : ''}

   ;-----------------------------------------------
   ; Call specific procedure to set config params
   ;-----------------------------------------------
   CASE sensorOption OF
       ; NOAA-18/AMSUA&MHS
       1: BEGIN
             configParam_NOAA_18_AMSUA_MHS, paramStruct   
	  END
       ; NOAA-19/AMSUA&MHS
       2: BEGIN
             configParam_NOAA_19_AMSUA_MHS, paramStruct 
	  END
       ; MetOp-A/AMSUA&MHS
       3: BEGIN
             configParam_MetOp_A_AMSUA_MHS, paramStruct      
	  END
       ; MetOp-B/AMSUA/MHS 
       4: BEGIN
             configParam_MetOp_B_AMSUA_MHS , paramStruct
	  END
       ; F16/SSMIS
       5: BEGIN
             configParam_F16_SSMIS, paramStruct
	  END
       ; F17/SSMIS
       6: BEGIN
             configParam_F17_SSMIS, paramStruct
	  END
       ; F18/SSMIS
       7: BEGIN
             configParam_F18_SSMIS, paramStruct
	  END
       ; NPP/ATMS
       8: BEGIN
             configParam_NPP_ATMS, paramStruct
	  END
       ; AQUA/AMSRE
       9: BEGIN
             configParam_AQUA_AMSRE, paramStruct
	  END
      ; GCOMW1/AMSR2
      10: BEGIN
             configParam_GCOMW1_AMSR2, paramStruct
	  END
      ; FY3/MWRI
      11: BEGIN
             configParam_FY3_MWRI, paramStruct
	  END
      ; FY3/MWHS/MWTS
      12: BEGIN
             configParam_FY3_MWHS_MWTS, paramStruct
	  END
      ; TRMM/TMI
      13: BEGIN
             configParam_TRMM_TMI, paramStruct
	  END
      ; GPM/GMI
      14: BEGIN
             configParam_GPM_GMI, paramStruct
	  END
      ; MT/MADRAS
      15: BEGIN
             configParam_MT_MADRAS, paramStruct
	  END
      ; MT/SAPHIR
      16: BEGIN
             configParam_MT_SAPHIR, paramStruct
	  END
      ; WindSat
      17: BEGIN
             configParam_WindSat, paramStruct
	  END
      ; AMVs
      18: BEGIN
            configParam_AMV, paramStruct
	  END
      ; GOES 13
      19: BEGIN
            configParam_GOES13_SNDR, paramStruct
	  END
      ; GOES 15 
      20: BEGIN
            configParam_GOES15_SNDR, paramStruct
	  END
      ; GOES 13
      21: BEGIN
            configParam_GOES13_IMGR, paramStruct
	  END
      ; GOES 15 
      22: BEGIN
            configParam_GOES15_IMGR, paramStruct
	  END
      ; AQUA AIRS (281)
      23: BEGIN
            configParam_AQUA_AIRS, paramStruct
	  END
      ; NPP CrIS (399)
      24: BEGIN
            configParam_NPP_CRIS, paramStruct
	  END
      ; AHI HIMAWARI 8 (10 IR channels)
      25: BEGIN
            configParam_H8_AHI, paramStruct
	  END
      ; ISS RAPIDSCAT
      26: BEGIN
            configParam_ISS_RAPIDSCAT, paramStruct
	  END
      ; ASCAT
      27: BEGIN
            configParam_MetOp_A_ASCAT, paramStruct
	  END
      ; option not available
      ELSE: BEGIN
             stop
	  END
   ENDCASE
END
