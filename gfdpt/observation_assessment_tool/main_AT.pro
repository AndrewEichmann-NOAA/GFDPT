;---------------------------------------------------------------------------------
; Name: main_AT.pro
;
; Type:  IDL Program
;
; Description: 
;   To read observed radiance data and simulated radiance data,
;   which is generated from GFS 6-h forecast, then do following steps:
;   to access SSMIS data quality. 
;     1. Plot observed radiance
;     2. Plot simulated radiance
;     3. Plot Bias ( observed radiance - simulated radiance )
;
; Author: Deyong Xu (RTI) @ JCSDA, 
;         Deyong.Xu@noaa.gov
; Version: Feb 27, 2014, DXu, Initial coding
;          May 23, 2014, EMaddy, added options for AMV assessment
;          Oct 20, 2014, MoharC, added options for Cloudy Radiance
;          Plotting
;          Oct 24, 2014, EMaddy, added IR sounder options
;          Oct 28, 2014, EMaddy, added options for IR clear-sky
;          filtering, QC array from rad files, ...
;
;---------------------------------------------------------------------------------
; Specify IDL plotting and util code to use.
@paths_idl.pro
; Specify locally-defined util code.
@AT_Util.pro
@emissret/train_reg.pro
;###########################
 mark_chooseSensor:
;###########################
PRINT, 'Choose instrument: '
PRINT,' 1 : NOAA-18/AMSUA&MHS'
PRINT,' 2 : NOAA-19/AMSUA&MHS'
PRINT,' 3 : MetOp-A/AMSUA&MHS'
PRINT,' 4 : MetOp-B/AMSUA/MHS'
PRINT,' 5 : F16/SSMIS'
PRINT,' 6 : F17/SSMIS'
PRINT,' 7 : F18/SSMIS'
PRINT,' 8 : NPP/ATMS'
PRINT,' 9 : AQUA/AMSRE'
PRINT,'10 : GCOMW1/AMSR2'
PRINT,'11 : FY3/MWRI'
PRINT,'12 : FY3/MWHS/MWTS'
PRINT,'13 : TRMM/TMI'
PRINT,'14 : GPM/GMI'
PRINT,'15 : MT/MADRAS'
PRINT,'16 : MT/SAPHIR'
PRINT,'17 : WindSat'
PRINT,'18 : SATWND AMV'
PRINT,'19 : GOES 13 SNDR'
PRINT,'20 : GOES 15 SNDR'
PRINT,'21 : GOES 13 IMGR'
PRINT,'22 : GOES 15 IMGR'
PRINT,'23 : AQUA AIRS(281 channel)'
PRINT,'24 : NPP CRIS(399 channel)'
PRINT,'25 : H8 AHI(10 IR channel)'
PRINT,'26 : ISS RAPIDSCAT'
PRINT,'27 : ASCAT'
sensorOption = 0S
READ, sensorOption

; Set flag to fill value: 999
optionFlag = 999
; Check to see if a right option is chosen.
FOR i = 1, 27 DO BEGIN 
   IF sensorOption eq i THEN BEGIN
      optionFlag = sensorOption 
      BREAK
   ENDIF
ENDFOR

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, mark_chooseSensor
ENDIF
                        
;-------------------------------------------
; Read config params for the sensor chosen
;-------------------------------------------
configSensorParam, sensorOption, paramStruct 

IF (sensorOption eq 18 or sensorOption eq 26 or sensorOption eq 27) THEN $
  GOTO, amv_assess

; Save config parameters to shorten name
MIN_LAT = paramStruct.MIN_LAT
MAX_LAT = paramStruct.MAX_LAT
MIN_LON = paramStruct.MIN_LON
MAX_LON = paramStruct.MAX_LON
radListFile1  = paramStruct.radListFile1
radListFile2  = paramStruct.radListFile2
sceneListFile = paramStruct.sceneListFile
biasListFile = paramStruct.biasListFile
stddevListFile = paramStruct.stddevListFile
MAX_FOV       = paramStruct.MAX_FOV
MAX_CHAN      = paramStruct.MAX_CHAN
MAX_QC        = 20L
sensorName    = paramStruct.sensorName
sensorID      = paramStruct.sensorID
clwCoeffPath  = paramStruct.clwCoeffPath
INT_FILL_Val = paramStruct.INT_FILL
FLOAT_FILL_Val = paramStruct.FLOAT_FILL
STRING_FILL_Val = paramStruct.STRING_FILL
; Filter out FILL values
indices_1 = WHERE(paramStruct.chanNumArr ne STRING_FILL_Val)
chanNumArr  = paramStruct.chanNumArr(indices_1)
indices_2 = WHERE(paramStruct.chanInfoArr ne STRING_FILL_Val)
chanInfoArr = paramStruct.chanInfoArr(indices_2)
indices_3 = WHERE(paramStruct.minBT_Values ne FLOAT_FILL_Val)
minBT_Values  = paramStruct.minBT_Values(indices_3)
indices_4 = WHERE(paramStruct.maxBT_Values ne FLOAT_FILL_Val)
maxBT_Values  = paramStruct.maxBT_Values(indices_4)
indices_5= WHERE(paramStruct.chPlotArr ne INT_FILL_Val)
chPlotArr   = paramStruct.chPlotArr(indices_5)
indices_6= WHERE(paramStruct.maxDTb_Values ne INT_FILL_Val)
maxDTb_Values = paramStruct.maxDTb_Values(indices_6)
CLW_THRESHOLD_MIN = paramStruct.CLW_THRESHOLD_MIN
CLW_THRESHOLD_MAX = paramStruct.CLW_THRESHOLD_MAX
RWP_THRESHOLD = paramStruct.RWP_THRESHOLD
GWP_THRESHOLD = paramStruct.GWP_THRESHOLD
GWP_THRESHOLD_MIN = paramStruct.GWP_THRESHOLD_MIN
GWP_THRESHOLD_MAX = paramStruct.GWP_THRESHOLD_MAX
date          = paramStruct.date
freq_unit     = paramStruct.freq_unit

; Create config params for rad plot
inputConfig_rad = { $
    chanNumArr : chanNumArr,$
    chanInfoArr : chanInfoArr,$
    prefix : '',$
    MIN_LAT : MIN_LAT,$
    MAX_LAT : MAX_LAT,$
    MIN_LON : MIN_LON,$
    MAX_LON : MAX_LON,$
    minBT_Values : minBT_Values,$
    maxBT_Values : maxBT_Values,$
    freq_unit  : freq_unit,$                
    date : date,$
    sensorName : sensorName,$
    nodeFlag : 0}

; Create config params for histogram plot
inputConfig_hist = { $
    chanNumArr : chanNumArr,$
    chanInfoArr : chanInfoArr,$
    prefix : '',$
    sensorName : sensorName,$
    MIN_LAT : MIN_LAT,$
    MAX_LAT : MAX_LAT,$
    MIN_LON : MIN_LON,$
    MAX_LON : MAX_LON,$
    minBT_Values : minBT_Values,$
    maxBT_Values : maxBT_Values,$
    freq_unit  : freq_unit,$
    date : date,$
    nodeFlag : 0, $
    trimOption : paramStruct.histogramOptions.trimOption, $
    trimValue : paramStruct.histogramOptions.trimValue, $
    useforclear : paramStruct.histogramOptions.useforclear}

inputConfig_emiss  = { $
    chanNumArr : chanNumArr,$
    chanInfoArr : chanInfoArr,$
    prefix : '',$
    sensorName : sensorName,$
    MIN_LAT : MIN_LAT,$
    MAX_LAT : MAX_LAT,$
    MIN_LON : MIN_LON,$
    MAX_LON : MAX_LON,$
    minBT_Values : minBT_Values,$
    maxBT_Values : maxBT_Values,$
    freq_unit  : freq_unit,$
    date : date,$
    nodeFlag : 0, $
    maxValue : paramStruct.emisOptions.maxValue, $
    useforclear : paramStruct.emisOptions.useforclear}


; Create config param for scatter plot
inputConfig_scat = { $
    chanNumArr : chanNumArr,$
    chanInfoArr : chanInfoArr,$
    maxDTb_Values : maxDTb_Values,$
    prefix : '',$
    sensorName : sensorName,$
    minBT_Values : minBT_Values,$
    maxBT_Values : maxBT_Values,$
    freq_unit  : freq_unit,$
    MIN_LAT : MIN_LAT,$
    MAX_LAT : MAX_LAT,$
    MIN_LON : MIN_LON,$
    MAX_LON : MAX_LON,$
    date : date }

;###########################
 mark_clw_option:
;###########################
PRINT, 'Choose CLW data source'
PRINT, '1 - CLW algorithm'
PRINT, '2 - CLW from NWP'
PRINT, '3 - CLW from both'
READ, clwOption
help, clwOption
IF ( clwOption NE 1 AND clwOption NE 2 AND clwOption NE 3) THEN BEGIN 
   PRINT, 'Wrong CLW option, choose again!'
   GOTO, mark_clw_option
ENDIf

;###########################
 mark_scene_option:
;###########################
PRINT, 'Choose Scene data source'
PRINT, '0 - New Scene dump file'
PRINT, '1 - Old Scene file'
READ, sceneOption

IF ( sceneOption NE 0 AND sceneOption NE 1 ) THEN BEGIN 
   PRINT, 'Wrong scene option, choose again!'
   GOTO, mark_scene_option
ENDIf
 
dataReformed = 0  ; set flag for reformed data 
;###########################
 mark_readAgain:
;###########################
PRINT, 'Read data again?'
PRINT, '1 - YES'
PRINT, '2 - NO, to reform data'
PRINT, '3 - NO, to plot data'
PRINT, '4 - NO, to create data assessment report (must have run steps 1-3 first)'

READ, readAgain
CASE readAgain OF
   1: GOTO, mark_read_data
   2: BEGIN
      IF (dataReformed eq 0) THEN BEGIN
        GOTO, mark_reform_data
      ENDIF ELSE BEGIN  
        PRINT, 'Data already reformed'
        GOTO,  mark_plotting_Unfiltered
      ENDELSE
   END   
   3: GOTO, mark_plotting
   4: GOTO, mark_DAR  ; not marklar
   ELSE: BEGIN & PRINT, 'Wrong option!!! Chose again...' & GOTO, mark_readAgain & END
ENDCASE

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, mark_readAgain
ENDIF


;###########################
 mark_read_data:
;###########################
;------------------------------------
; step 1: 
;   Read two lists of radiance files
;   and one list of  scene files
;------------------------------------
readlist, radListFile1, radFileList1, nFilesRad1
readlist, radListFile2, radFileList2, nFilesRad2
readlist, sceneListFile, sceneFileList, nFileScene

; Get number of radiance files in each list/array.
nRadFiles1  = n_elements(radFileList1)
nRadFiles2  = n_elements(radFileList2)
nSceneFiles = n_elements(sceneFileList)

; Make sure that numbers of radiance files and 
; scenef files are equal and not 0.
IF ( ( nRadFiles1 NE nRadFiles2 )  || $ 
     ( nRadFiles1 NE nSceneFiles ) || $ 
     ( nRadFiles1 EQ 0 ) ) THEN BEGIN
   PRINT,'Error: Number of Rad files in two list files NOT match'
;   STOP
ENDIF

; Save number of rad files (orbits)
nOrbits=nRadFiles1

;nOrbits=1
;---------------------------------------------
; step 2:
;   Define data structures and initalize data 
;---------------------------------------------
initializeAll, MAX_FOV, nOrbits, MAX_CHAN, MAX_QC, $
   radObs, radSim, sceneData, $
   refRadObs, refRadSim, refSceneData

PRINT, "Begin readRadFile  =========="
;-------------------------------------------
; step 3:
;   Read radiances (measurements) from List1
;   Read radiances (simulated) from List2
;   Read scene file (GFS 6-hr forecast)
;-------------------------------------------
;print, "****************    ",  sceneFileList
readRadFile, nOrbits, MAX_FOV, MAX_CHAN, MAX_QC,     $
   radFileList1, radFileList2, sceneFileList, $
   radObs, radSim, sceneData, oldscene=sceneOption

nScanPos=max(radObs.scanPos)

PRINT, "done with readRadFile  =========="

;-------------------------------------------
; step 4:
;   Reform data
;-------------------------------------------
;###########################
 mark_reform_data:
;###########################
reformArray, MAX_FOV, nOrbits,  $
   sensorID, radObs, radSim, refRadObs, refRadSim, $
   sceneData, refSceneData    

;---delete temporary memory structures
i1 = SIZE(TEMPORARY(radObs))
i1 = SIZE(TEMPORARY(radSim))
i1 = SIZE(TEMPORARY(sceneData))
dataReformed = 1

 mark_plotting:

nchan = N_ELEMENTS(chPlotArr)
STR_IDL_TO_TEX, freq_unit
frqunit = freq_unit
;---create structures to hold statistics for DAR output 
stats_ClearSky   = {chan: LONG(ChanNumArr[chPlotArr]), $
                    chanfreq: ChanInfoArr[chPlotArr], $
                    freq_unit: frqunit, $
                    bias_all: FLTARR(nchan), sdv_all: FLTARR(nchan), rms_all: FLTARR(nchan), $
                    correl_all: FLTARR(nchan), n_all: LONARR(nchan), $
                    bias_asc: FLTARR(nchan), sdv_asc: FLTARR(nchan), rms_asc: FLTARR(nchan), $
                    correl_asc: FLTARR(nchan), n_asc: LONARR(nchan), $
                    bias_desc: FLTARR(nchan), sdv_desc: FLTARR(nchan), rms_desc: FLTARR(nchan), $
                    correl_desc: FLTARR(nchan), n_desc: LONARR(nchan)}
stats_Unfiltered = stats_ClearSky  ; copy empty structure for unfiltered
stats_Precip     = stats_ClearSky
stats_Cloudy     = stats_ClearSky

PLOT_UNFILTERED = 1
PLOT_CLRSKY     = 1
PLOT_FILTER     = 1
PLOT_CLDSKY     = 0
PLOT_PCPSKY     = 0
PLOT_SCENE      = 0
PLOT_ICESKY     = 0
;refRadObs_save = refRadObs

;-------------------------------
; test if image directory exists
; if not create it
;-------------------------------
IF (NOT FILE_TEST(paramStruct.imageDir,/DIRECTORY)) THEN BEGIN
  cmd = 'mkdir -p ' + paramStruct.imageDir
  PRINT, cmd, FORMAT="(1x,'Image Directory does not exist.  Creating directory:',/,3x,a)"
  SPAWN, cmd 
ENDIF 
;-----------------------------------------
; step 5: 
;   Plot radiances (observed + simulated)
;-----------------------------------------

;coef_file = 'emissret/gpm_gmi_2014-08-10_emisreg.sav'
;coef_file = 'emissret/f18_ssmis_2013-01-20_emisreg.sav'

;---update as needed with regression emissivity -
;   stub for install of emissivity regression for all
;   sensors
IF (sensorID eq 'f18' or sensorID eq 'gmi') THEN BEGIN
;  APPLY_SIMPLE_EMISREG, refRadObs, refSceneData, coef_file, emisReg
;  refSceneData.emissanlvec = emisreg
   emisVec = refSceneData.emissvec-refSceneData.emissvec
   tbc=refRadObs.tb
   retrieve_edr, 'em', tbc, refSceneData.sfcTypeVec, clwCoeffPath, sensorID, $
                 refRadObs.angle, refRadObs.scanPos, refRadObs.lat, emisVec
   refSceneData.emissanlvec = emisVec
ENDIF ELSE  refSceneData.emissanlvec = refSceneData.emissvec

IF (PLOT_UNFILTERED EQ 1) THEN BEGIN
    ;###########################
     mark_plotting_Unfiltered:
    ;###########################
    ; Plot radiances and radiance difference

    inputConfig_rad.prefix = paramStruct.imageDir+sensorName + '_Rad_plot_' 
    inputConfig_rad.nodeFlag = 0
    plotRad, chPlotArr, inputConfig_rad, refRadObs, refRadSim
    
    inputConfig_rad.nodeFlag = 1
    plotRad, chPlotArr, inputConfig_rad, refRadObs, refRadSim

    ; Plot various scattering plots
    inputConfig_scat.prefix = paramStruct.imageDir+sensorName + '_ScatPlot' 
    plotScattering, chPlotArr, inputConfig_scat, $
      refRadObs, refRadSim, refSceneData

ENDIF
IF (PLOT_CLRSKY EQ 1) THEN BEGIN
    ;###########################
     mark_plotting_ClearSky:
    ;###########################

    ; Generate clw using clw algorithm
    IF ( clwOption EQ 1 OR clwOption EQ 3) THEN BEGIN 
       IF (sensorID eq 'CRIS' or sensorID eq 'IASI' or $
           sensorID eq 'AIRS' or sensorID eq 'AHI') THEN BEGIN
          ;---use the clwVec/gwpVec as a proxy to filter
          ;   cases
          tmpSize = N_ELEMENTS(refSceneData.clwVec)
          clwVec = FLTARR(tmpSize)+10.
          gwpVec = FLTARR(tmpSize)+10.
          tb = refradObs.tb
          ;---currently only works for ocean cases
          sfctype = 0
          HSIR_FILTER, sensorID, refradObs.tb, refradObs.qc, $
                       refSceneData.sfctypevec, refSceneData.tskinvec, $
                       refRadObs.solangle, refRadObs.angle, $
                       sfctype, refRadObs.lat, iclear, nclear
          
          ;---mark clear for cases meeting criteria
          IF (nclear gt 0) THEN clwVec[iclear] = 0.0 
          IF (nclear gt 0) THEN gwpVec[iclear] = 0.0 
          print, MIN(refRadObs.lat[iclear]), MAX(refRadObs.lat[iclear]), $
                 PERCENTILE_RANK(refRadObs.lat[iclear],[0.,1,99,100]), $
                 PERCENTILE_RANK(refSceneData.sfctypevec[iclear],[0.,1,99,100])
          ;; set_plot, 'X'
          ;; erase
          ;; !P.MULTI =[0,1,1]
          ;; LOADCT, 39            ;, /SILENT
          ;; TVLCT, r, g, b, /GET
          
          ;; r(0)=255   & g(0)=255   & b(0)=255 ; Load White Color          - 0   for white background 
          ;; r(255)=0   & g(255)=0   & b(255)=0 ; Load Black Color          - 255 for black color
          ;; r(254)=255 & g(254)=255 & b(254)=255 ; Load White Color          - 254 for Missing Data
          ;; r(253)=215 & g(253)=215 & b(253)=215 ; Load Grey Color (20%)     - 253 for Land/Ocean Coverage
          ;; r(252)=98  & g(252)=98  & b(252)=98  ; Load Grey Color (70%)     - 252 for non-convergent points(qc fail)
          ;; r(251)=176 & g(251)=196 & b(251)=222 ; Load Lightsteelblue Color - 251 for non-reported points(snow/ice prevents)
          
          ;; TVLCT, r, g, b
          ;; colorBlack = 255
          ;; plot, refRadObs.lon[iclear], refRadObs.lat[iclear], PSYM=4, $
          ;;       SYMSIZE=0.25, COLOR=colorBlack, XRA=[-180., 180.], YRA=[-90.,90.]
          
       ENDIF ELSE BEGIN
        ; Find out array size of refSceneData.clwVec
        ; We create a new clw array of the same size as refSceneData.clwVec
        tmpSize = N_ELEMENTS(refSceneData.clwVec)
        clwVec = FLTARR(tmpSize)
        gwpVec = FLTARR(tmpSize)

        print,'Apply radiometric bias correction for CLW regression?'
        print,'1 - YES'
        print,'2 - NO'
        read,answerBias
        tbc=refRadObs.tb

        if (answerBias eq 1) then begin
        
          ; Read bias coefficients
           ReadBias, ParamStruct.biasFile, nChan,nPosBias,Freq,meanBias,Slope,Intercept,meanTbSim,meanTbObs
           biasByChan=make_array(nChan,/integer, value=1)
           tbc    = make_array(tmpSize,nChan,/float,value=-999.)

          ; Bias correct brightness temperatures since training was done on simulated data
           for i=0,tmpSize-1 do begin
             if (refSceneData.sfcTypeVec[i] lt 0) then CONTINUE
               apply_bias, refRadObs.tb[i,*], tbcTemp, refRadObs.scanPos[i], nScanPos, nPosBias, meanBias, $
                Slope, Intercept, nChan, -1, 0, refSceneData.sfcTypeVec[i],biasByChan, biasByChan, $
               biasByChan, biasByChan
             tbc[i,*]=tbcTemp[*]
           endfor

        endif

        retrieve_edr, 'clw', tbc, refSceneData.tskinvec, refSceneData.sfcTypeVec, clwCoeffPath, sensorID, $
                      refRadObs.angle, refRadObs.scanPos, refRadObs.lat, clwVec

        retrieve_edr, 'gwp', tbc, refSceneData.tskinvec, refSceneData.sfcTypeVec, clwCoeffPath, sensorID, $
                      refRadObs.angle, refRadObs.scanPos, refRadObs.lat, gwpVec


;        SI_ch18_8=refradobs.tb[*,17]-refradobs.tb[*,7]

     ENDELSE
    ENDIF
    ; Define filter to get clear sky points via scene data 
    IF ( clwOption EQ 1 ) THEN filterClearSky = WHERE(clwVec LT CLW_THRESHOLD_MIN AND $
                                                      gwpVec LT GWP_THRESHOLD_MIN AND $
                                                      refSceneData.sfcTypeVec eq 0,nfilterClearSky)
    IF ( clwOption EQ 2 ) THEN filterClearSky = WHERE(refSceneData.clwVec GE 0. AND $
                                                      refSceneData.clwVec LT CLW_THRESHOLD_MIN and $
                                                      refSceneData.sfcTypeVec eq 0, nfilterClearSky)
    IF ( clwOption EQ 3 ) THEN filterClearSky = WHERE(refSceneData.clwVec GE 0. AND $
                                                      refSceneData.clwVec LT CLW_THRESHOLD_MIN AND $
                                                      clwVec LT CLW_THRESHOLD_MIN AND $
                                                      gwpVec LT GWP_THRESHOLD_MIN AND $
                                                      refSceneData.sfcTypeVec eq 0, $
                                                      nfilterClearSky)
    help, filterClearSky

    ihisttrim = WHERE(paramStruct.histogramOptions.trimOption ne 0,nhisttrim)

    IF ( nhisttrim gt 0 ) THEN BEGIN
     nbin = 1000L
     sfctype = refSceneData.sfctypeVec
     inputConfig_hist.prefix = paramStruct.imageDir + sensorName +  '_all_ocean_histograms'
     
     ComputeHistogramStats_OAT, nbin, chplotArr, filterClearSky, inputConfig_hist, $
                                sfctype, refRadObs, refRadSim, histBias, histPeak, histStdv, filter_clear
     
      
     filterClearSky_old = filterClearSky
     filterClearSky = filter_clear
     nfilterClearSky = N_ELEMENTS(filterClearSky)
    ENDIF

    help, nfilterClearSky 
    iemiss = WHERE(paramStruct.emisOptions.useforclear gt 0,nemiss)
    help, nemiss
    IF (nemiss gt 0) THEN BEGIN
;     refSceneData
     sfctype = refSceneData.sfctypeVec
     EmissivityTest, filterClearSky, chPlotArr, inputConfig_emiss, $
                          refSceneData, sfctype, filter_clear
     filterClearSky_old = filterClearSky
     filterClearSky = filter_clear
     nfilterClearSky = N_ELEMENTS(filterClearSky)
    ENDIF
    help, nfilterClearSky 
   
    IF (PLOT_FILTER EQ 1) THEN BEGIN
       IF (clwOption eq 1 or clwOption eq 3) THEN BEGIN
;          sensorName = inputConfig.sensorName
          ;; MIN_LAT = inputConfig.MIN_LAT
          ;; MAX_LAT = inputConfig.MAX_LAT
          ;; MIN_LON = inputConfig.MIN_LON
          ;; MAX_LON = inputConfig.MAX_LON
          ;; date = inputConfig.date
          ;; freq_unit  = inputConfig.freq_unit
          index = WHERE(refRadObs.lat ge MIN_LAT and $
                        refRadObs.lat le MAX_LAT and $
                        refRadObs.lon ge MIN_LON and $
                        refRadObs.lon le MAX_LON and $
                        clwVec ge 0. and gwpVec ge 0. and $
                        refSceneData.sfctypevec eq 0,nindex)
          IF (nindex ne 0) THEN BEGIN
            order=!ORDER 
            !ORDER = 0
            img_name = paramStruct.imageDir+sensorName + '_CLWfilter.png' 
            tit = STRCOMPRESS(sensorName + ' CLW regression :' + date )
            idx = WHERE(clwVec gt 1. and clwVec lt 5.,nidx)
            clwVec[idx] = 1.0
            idx = WHERE(clwVec gt 10.,nidx)
            IF(nidx ne 0) THEN clwVec[idx] = 0
            IF(nidx ne 0) THEN gwpVec[idx] = 0

            mapPlot_png, MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
                         refRadObs.lat, refRadObs.lon, index, tit, 0, 1., clwVec, $
                         'clw, (g/m!U2!N)', 0.6, $
                         8, 1, 0, '(f7.2)', 8, img_name, color_table_index=17
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            
            img_name = paramStruct.imageDir+sensorName + '_GWPfilter.png' 
            tit = STRCOMPRESS(sensorName + ' GWP regression : ' + date )
            idx = WHERE(gwpVec gt .5,nidx)
            gwpVec[idx] = 0.5
            mapPlot_png, MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
                         refRadObs.lat, refRadObs.lon, index, tit, 0., .5, gwpVec, $
                         'gwp, (g/m!U2!N)', 0.6, $
                         8, 1, 0, '(f7.2)', 8, img_name, color_table_index=17
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            !ORDER = order
          ENDIF
       ENDIF   
       IF (nemiss gt 0) THEN BEGIN
          emis = REFORM(refSceneData.emissanlvec[*,iemiss[1]])
          index = WHERE(refRadObs.lat ge MIN_LAT and $
                        refRadObs.lat le MAX_LAT and $
                        refRadObs.lon ge MIN_LON and $
                        refRadObs.lon le MAX_LON and $
                        emis ge 0. and emis le 1.,nindex)
          IF (nindex ne 0) THEN BEGIN
            order=!ORDER 
            cstr = chanInfoArr[iemiss[1]] + 'GHz '
            tit = STRCOMPRESS(sensorName + ' emissivity regression @ ' + cstr + ': ' + date )
            !ORDER = 0
            idx = WHERE(emis gt paramStruct.emisOptions.maxValue[iemiss[1]]*1.1,nidx)
            IF (nidx ne 0) THEN emis[idx] = paramStruct.emisOptions.maxValue[iemiss[1]]*1.1
            img_name = paramStruct.imageDir+sensorName + '_emisfilter.png' 
            mapPlot_png, MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
                         refRadObs.lat, refRadObs.lon, index, tit, 0, $
                         paramStruct.emisOptions.maxValue[iemiss[1]]*1.1, emis, $
                         'emis, (1)', 0.6, $
                         8, 1, 0, '(f7.2)', 8, img_name, color_table_index=17
            map_continents,/continents,/noborder,/hires,/usa,fill_continents=0,color=18
            !ORDER = order
          ENDIF
       ENDIF
    ENDIF

    ; Generated filtered data
    IF (nfilterClearSky ne 0) THEN BEGIN
        generateConditionalData, filterClearSky, refRadObs, refRadSim, refSceneData, $
          refRadObs_ClearSky, refRadSim_ClearSky, refSceneData_ClearSky
        
        inputConfig_rad.prefix = paramStruct.imageDir+sensorName + '_Rad_plot_ClrSky_' 
        inputConfig_rad.nodeFlag = 0
        plotRad, chPlotArr, inputConfig_rad, refRadObs_ClearSky, refRadSim_ClearSky  
        
        inputConfig_rad.nodeFlag = 1
        plotRad, chPlotArr, inputConfig_rad, refRadObs_ClearSky, refRadSim_ClearSky
        
                                ; Plot various scattering plots
        inputConfig_scat.prefix = paramStruct.imageDir+sensorName + '_ScatPlot_ClrSky_' 
        plotScattering, chPlotArr, inputConfig_scat, $
          refRadObs_ClearSky, refRadSim_ClearSky, refSceneData_ClearSky

    ENDIF 

 ENDIF


IF (PLOT_CLDSKY EQ 1) THEN BEGIN
    ;###########################
     mark_plotting_CloudySky:
    ;###########################
    ; Define filter to get clear sky points via scene data 
    IF ( clwOption EQ 1 ) THEN BEGIN
        filterCloudySky = WHERE(clwVec GT CLW_THRESHOLD_MIN $
                                AND clwVec LE CLW_THRESHOLD_MAX,nFilterCloudySky)
    ENDIF
    IF ( clwOption EQ 2 ) THEN BEGIN
        filterCloudySky = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MIN $
                                AND refSceneData.clwVec LE CLW_THRESHOLD_MAX,nFilterCloudySky)
    ENDIF
    IF ( clwOption EQ 3 ) THEN BEGIN
        filterCloudySky = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MIN $
                                AND refSceneData.clwVec LE CLW_THRESHOLD_MAX AND clwVec GT CLW_THRESHOLD_MIN $
                                AND clwVec LE CLW_THRESHOLD_MAX,nFilterCloudySky)
    ENDIF
    help, filterCloudySky
    
    IF (nfilterCloudySky ne 0) THEN BEGIN
        ; Generated filtered data
        generateConditionalData, filterCloudySky, refRadObs, refRadSim, refSceneData, $
          refRadObs_CloudySky, refRadSim_CloudySky, refSceneData_CloudySky
        
        inputConfig_rad.prefix = paramStruct.imageDir+sensorName + '_Rad_plot_CldSky_' 
        inputConfig_rad.nodeFlag = 0
        plotRad, chPlotArr, inputConfig_rad, refRadObs_CloudySky, refRadSim_CloudySky
        
        inputConfig_rad.nodeFlag = 1
        plotRad, chPlotArr, inputConfig_rad, refRadObs_CloudySky, refRadSim_CloudySky
        
        ; Plot various scattering plots
        inputConfig_scat.prefix = paramStruct.imageDir+sensorName + '_ScatPlot_CldSky_' 
        plotScattering, chPlotArr, inputConfig_scat,$
          refRadObs_CloudySky, refRadSim_CloudySky, refSceneData_CloudySky
    ENDIF
ENDIF

IF (PLOT_PCPSKY EQ 1) THEN BEGIN
    ;###########################
     mark_plotting_Precip:
    ;###########################

    ; Define filter to get precipitation points via scene data 
    IF ( clwOption EQ 1 ) THEN filterPrecip = WHERE(clwVec GT CLW_THRESHOLD_MAX AND gwpVec GT GWP_THRESHOLD_MAX,nFilterPrecip)
    IF ( clwOption EQ 2 ) THEN filterPrecip = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MAX,nFilterPrecip)
    IF ( clwOption EQ 3 ) THEN filterPrecip = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MAX AND clwVec GT CLW_THRESHOLD_MAX AND $
                                                    gwpVec GT GWP_THRESHOLD_MAX,nFilterPrecip)
    help, filterPrecip

    IF (nfilterPrecip ne 0) THEN BEGIN
        ; Generated filtered data
        generateConditionalData, filterPrecip, refRadObs, refRadSim, refSceneData, $
          refRadObs_Precip, refRadSim_Precip, refSceneData_Precip
        
        inputConfig_rad.prefix = paramStruct.imageDir+sensorName + '_Rad_plot_Precp_' 
        inputConfig_rad.nodeFlag = 0
        plotRad, chPlotArr, inputConfig_rad, refRadObs_Precip, refRadSim_Precip
        
        inputConfig_rad.nodeFlag = 1
        plotRad, chPlotArr, inputConfig_rad, refRadObs_Precip, refRadSim_Precip
   
        ; Plot various scattering plots
        inputConfig_scat.prefix = paramStruct.imageDir+sensorName + '_ScatPlot_Prcp_' 
        plotScattering, chPlotArr, inputConfig_scat, $
          refRadObs_Precip, refRadSim_Precip, refSceneData_Precip
    ENDIF 
ENDIF

IF (PLOT_ICESKY EQ 1) THEN BEGIN
    ;###########################
     mark_plotting_Graupel:
    ;###########################

    ; Define filter to get precipitation points via scene data 
    IF ( clwOption EQ 1 ) THEN filterPrecip = WHERE(clwVec GT CLW_THRESHOLD_MAX AND gwpVec GT GWP_THRESHOLD_MAX,nFilterPrecip)
    IF ( clwOption EQ 2 ) THEN filterPrecip = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MAX AND refSceneData.clwVec GT RWP_THRESHOLD AND refSceneData.clwVec GT GWP_THRESHOLD, nFilterPrecip)
    
    IF ( clwOption EQ 3 ) THEN filterPrecip = WHERE(refSceneData.clwVec GT CLW_THRESHOLD_MAX AND clwVec GT CLW_THRESHOLD_MAX AND $
                                                    gwpVec GT GWP_THRESHOLD_MAX,nFilterPrecip)
    help, filterPrecip

    IF (nfilterPrecip ne 0) THEN BEGIN
        ; Generated filtered data
        generateConditionalData, filterPrecip, refRadObs, refRadSim, refSceneData, $
          refRadObs_Precip, refRadSim_Precip, refSceneData_Precip
        
        inputConfig_rad.prefix = paramStruct.imageDir+sensorName + '_Rad_plot_Precp_' 
        inputConfig_rad.nodeFlag = 0
        plotRad, chPlotArr, inputConfig_rad, refRadObs_Precip, refRadSim_Precip
        
        inputConfig_rad.nodeFlag = 1
        plotRad, chPlotArr, inputConfig_rad, refRadObs_Precip, refRadSim_Precip
   
        ; Plot various scattering plots
        inputConfig_scat.prefix = paramStruct.imageDir+sensorName + '_ScatPlot_Prcp_' 
        plotScattering, chPlotArr, inputConfig_scat, $
          refRadObs_Precip, refRadSim_Precip, refSceneData_Precip
    ENDIF 
ENDIF


IF (PLOT_SCENE EQ 1) THEN BEGIN
   ; ###########################
     mark_plotting_Scene:
    ;###########################




ENDIF
;########
mark_DAR: 
;########

;---generate statistics for DAR tabular output (only unfiltered and
;   clear-sky at this point.
generateStats_for_DAR, refRadObs, refRadSim, refSceneData.sfctypevec, $
                       chPlotArr, inputConfig_rad, stats_Unfiltered

IF (PLOT_CLRSKY EQ 1) THEN BEGIN
generateStats_for_DAR, refRadObs_ClearSky, refRadSim_ClearSky, $
                       refSceneData_ClearSky.sfctypeVec, $
                       chPlotArr, inputConfig_rad, stats_ClearSky
ENDIF

if (PLOT_CLDSKY) THEN BEGIN  ; make sure this was called 
   generateStats_for_DAR, refRadObs_CloudySky, refRadSim_CloudySky, $
                          refSceneData_CloudySky.sfctypeVec, $
                          chPlotArr, inputConfig_rad, stats_Cloudy
ENDIF 

if (PLOT_PCPSKY) THEN BEGIN  ; make sure this was called 
   generateStats_for_DAR, refRadObs_Precip, refRadSim_Precip, $
                          refSceneData_Precip.sfctypeVec, $
                          chPlotArr, inputConfig_rad, stats_Precip
ENDIF 

if (PLOT_ICESKY) THEN BEGIN  ; make sure this was called 
   generateStats_for_DAR, refRadObs_Precip, refRadSim_Precip, $
                          refSceneData_Precip.sfctypeVec, $
                          chPlotArr, inputConfig_rad, stats_Precip
ENDIF 

;; SET_PLOT, 'X'
;; !p.font = -1
;plotSpecStat, stats_ClearSky
; stop
;---create data assessment report
inputConfig_DAR = { $
    chanNumArr : chanNumArr,$
    chanInfoArr : chanInfoArr,$
    date : date,$
    imageDir: paramStruct.imageDir, $
    sensorName : sensorName, $
    stats_ClearSky: stats_ClearSky, $
    stats_Unfiltered: stats_Unfiltered, $
    stats_Precip: stats_Precip, $
    stats_Cloudy: stats_Cloudy}
DAR_setupandrun, inputConfig_DAR

;###########################
 mark_plotting_Bias_Stddev:
;###########################
;---Read bias filenames and read the biases
readlist, biasListFile, biasFileList, nFileBias
readlist, stddevListFile, stddevFileList, nFileStddev

nBiasFiles = N_ELEMENTS(biasFileList)
nStddevFiles = N_ELEMENTS(stddevFileList)

IF ( nBiasFiles NE nStddevFiles ) THEN BEGIN 
   PRINT,'Error: Number of Bias files and Stddev files NOT match'
   STOP
ENDIF

plotBiasAndAvg, sensorName, biasFileList, stddevFileList

GOTO, amv_end  ; end processing for radiance assessment

;---AMV assessment begin processing

;###########################
amv_assess: 
;###########################

;###########################
 amv_mark_readAgain:
;###########################
PRINT, 'Read AMV data again?'
PRINT, '1 - YES'
PRINT, '2 - NO, to plot unfiltered data'
PRINT, '3 - NO, to plot filtered GSI QC''d blacklisted '
PRINT, '4 - NO, to plot low level filtered'
PRINT, '5 - NO, to plot high level filtered'

READ, readAgain
CASE readAgain OF
   1: GOTO, amv_mark_read_data
   2: GOTO, amv_mark_plotting_Unfiltered
   3: GOTO, amv_mark_plotting_Filtered 
   4: GOTO, amv_mark_plotting_LowLevel 
   5: GOTO, amv_mark_plotting_HighLevel 
   ELSE: BEGIN 
     PRINT, 'Wrong option!!! Chose again...' 
     GOTO, amv_mark_readAgain 
   END
ENDCASE

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, amv_mark_readAgain
ENDIF

;###########################
amv_mark_read_data: 
;###########################
;------------------------------------
; step 1: 
;   Read two lists of AMV files
;------------------------------------

ReadList, paramStruct.AMVsceneListFile, AMVFileList, nAMVFiles
ReadList, paramStruct.NWPsceneListFile, NWPFileList, nNWPFiles

IF (nNWPFiles ne nAMVFiles) THEN BEGIN
  msg = 'Number of AMV Files <> Number of NWP Files'
  PRINT, msg, FORMAT="(a)"
  stop
ENDIF

ReadAMVList, AMVFileList, sceneamv
ReadAMVList, NWPFileList, nwpamv

;###########################
amv_mark_plotting_Unfiltered: 
;###########################

paramStruct.amvOptions.qualityMark  = 0
paramStruct.amvOptions.gsiblacklist = 0
paramStruct.amvOptions.highlevel    = 0
paramStruct.amvOptions.lowlevel     = 0
AMV_Assessment, sceneamv, nwpamv, paramStruct.amvOptions

GOTO, amv_end

;###########################
amv_mark_plotting_Filtered: 
;###########################

paramStruct.amvOptions.qualityMark  = 1
paramStruct.amvOptions.gsiblacklist = 1
paramStruct.amvOptions.lowlevel     = 0
paramStruct.amvOptions.highlevel    = 0
AMV_Assessment, sceneamv, nwpamv, paramStruct.amvOptions

GOTO, amv_end

;###########################
amv_mark_plotting_LowLevel: 
;###########################

paramStruct.amvOptions.qualityMark  = 1
paramStruct.amvOptions.gsiblacklist = 1
paramStruct.amvOptions.lowlevel     = 1
paramStruct.amvOptions.highlevel    = 0
AMV_Assessment, sceneamv, nwpamv, paramStruct.amvOptions

GOTO, amv_end

;###########################
amv_mark_plotting_HighLevel: 
;###########################

paramStruct.amvOptions.qualityMark  = 1
paramStruct.amvOptions.gsiblacklist = 1
paramStruct.amvOptions.highlevel    = 1
paramStruct.amvOptions.lowlevel     = 0
AMV_Assessment, sceneamv, nwpamv, paramStruct.amvOptions

;###########################
amv_end: 
;###########################

PRINT,'End of processing...'

END
