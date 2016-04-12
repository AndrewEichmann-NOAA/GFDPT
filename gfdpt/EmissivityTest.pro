;===============================================================
;+
; Name:       EmissivityTest
;
; Type:       IDL Subroutine
;
; Description: Produce list of clear indices using thresholds of the 
;              emissivity determined by regression of observed cloudy
;              data inversion
;              of the RTE using observations and CRTM for forward calculations
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -initfilter_clear  Input            Initial clear filter 
;      -inputConfig       Input            User configurable
;      parameters
;      -refSceneData      Input            sceneData type
;      -sfctype           Input            Surface classification
;      -filter_clear      Output           index of clear cases
;
; History:  
;        01-22-2015    ESM - Created
;
; Limitations: 
;        Clear filter currently only applies over ocean.
;-
;===============================================================

PRO EmissivityTest, initfilter_clear, chplotArr, inputConfig, $
                        refSceneData, sfctype, filter_clear
    ; Get input config params
    chanNumArr = inputConfig.chanNumArr
    chanInfoArr = inputConfig.chanInfoArr
    prefix  = inputConfig.prefix
    MIN_LAT = inputConfig.MIN_LAT
    MAX_LAT = inputConfig.MAX_LAT
    MIN_LON = inputConfig.MIN_LON
    MAX_LON = inputConfig.MAX_LON
    minBT_Values = inputConfig.minBT_Values
    maxBT_Values = inputConfig.maxBT_Values
    date = inputConfig.date
    nodeFlag = inputConfig.nodeFlag
    trimValue = inputConfig.maxValue
    useforclear = inputConfig.useforClear
    ; Get sensor name 
    sensorName = inputConfig.sensorName

    ; Get num of channels to plot
    nchan = N_ELEMENTS(chPlotArr)
    
    ; we only want to consider ocean cases for bias determination 
    ifilter_all = LINDGEN(N_ELEMENTS(initfilter_Clear))

    allfilter_clear = initfilter_clear
    
    countTest = 0L
    ; Loop thru. surface type to plot
    FOR isfctype = 0, 0 DO BEGIN
      ; Loop thru. channels to plot
      FOR ichan=0,nchan-1 DO BEGIN
        ; check for good data
        filter_Alln = WHERE(sfctype[initfilter_clear] eq isfctype, nAlln )
        ; no good data continue 
        help, nAlln
        IF (nAlln eq 0) THEN continue 
        ;---emissivity analytic value threshold
;        demis = REFORM(refSceneData.EmissVec[initfilter_clear[filter_Alln], chplotArr[ichan]] - $
;                       refSceneData.EmissAnlVec[initfilter_clear[filter_Alln], chplotArr[ichan]] )
        ;---regression emissivity test
        demis = REFORM( refSceneData.EmissAnlVec[initfilter_clear[filter_Alln], chplotArr[ichan]] )
        i_trim = WHERE(demis le trimValue[iChan],ni_trim)
        help, iChan, ni_trim
        IF (ni_trim eq 0) THEN continue 
        
        IF (useforclear[iChan] eq 1) THEN BEGIN
          IF (countTest eq 0) THEN i_clear2 = initfilter_clear[filter_Alln[i_trim]] ELSE $
             i_clear2 = INTERSECT(i_clear2,initfilter_clear[filter_Alln[i_trim]])
          countTest += 1  ; add to counter
        ENDIF
;        help, countTest
;        help, filter_clear
;        help, i_clear2
      ENDFOR
    ENDFOR
    ;---if user wanted determined "clear" pass back indices
    ;   otherwise, return initial filter
    IF (countTest eq 0) THEN filter_clear = initfilter_clear ELSE filter_clear = i_clear2
END
