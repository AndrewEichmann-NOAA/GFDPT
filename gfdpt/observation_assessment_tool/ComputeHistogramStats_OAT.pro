;===============================================================
;+
; Name:       ComputeHistogramStats_OAT
;
; Type:       IDL Subroutine
;
; Description: Create histogram of data and compute bias, stdev, peak,
; and optionally produce clear indices determined by histogram adjustment
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -nbin              Input            Number of bins for
;      histogram
;      -chPlotArr         Input            Channels to plot
;      -initfilter_clear  Input            Initial clear filter from
;      CLW
;      -inputConfig       Input            User configurable
;      parameters
;      -sfctype           Input            Surface classification
;      -refRadObs         Input            Observation Radiance Structure 
;      -refRadSim         Input            Simulated Radiance Structure 
;      -histBias (nchan)  Output           Histogram bias        
;      -histStdv (nchan)  Output           Histogram stddev            
;      -histPeak (nchan)  Output           Histogram Peak location
;      -filter_clear      Output           index of clear cases
;      determined by histogram adjustment            
;
; History:  
;        06-24-2014    Kevin Garrett, Rti @ NOAA/NESDIS/STAR/JCSDA
;        07-03-2014    Eric S. Maddy, Rti @ NOAA/NESDIS/STAR/JCSDA
;                      Modified for use in the OAT --added histogram
;                      adjustment clear detection.
;        07-07-2014    ESM - added options for trim type, channel
;                            selection for clear filter, cleaned up
;                            some code.
;
; Limitations: 
;        Clear filter using the histogram adjustment technique 
;        currently only applies over ocean.
;-
;===============================================================

PRO ComputeHistogramStats_OAT, nbin, chplotArr, initfilter_clear, inputConfig, $
                               sfctype, refRadObs, refRadSim, histBias, histPeak, histStdv, $
                               filter_clear
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
    trimOption = inputConfig.trimOption
    trimValue  = inputConfig.trimValue
    useforclear = inputConfig.useforClear
    freq_unit   = inputConfig.freq_unit
    ; Get sensor name 
    sensorName = inputConfig.sensorName

    ; Get num of channels to plot
    nchan = N_ELEMENTS(chPlotArr)
    
    ;---stats output arrays
    histBias=fltarr(nchan,4)
    histPeak=fltarr(nchan,4)
    histStdv=fltarr(nchan,4)

    !P.MULTI = [0,2,2]
    ; Set XSIZE and YSIZE for PS.
    xSizeVal=18
    ySizeVal=18

    ; Save graphics in PS
    SET_PLOT, 'PS'

    imageName = STRCOMPRESS(prefix + '.ps',/remove_all)
    LOADCT, 39
    !P.FONT=0
    DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
            XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=1, YOFFSET=1,  $
            /PORTRAIT, FONT_SIZE=8, /BOLD, /COURIER, ENCAPSULATED=0

    ; we only want to consider ocean cases for bias determination 
    ifilter_all = LINDGEN(N_ELEMENTS(initfilter_Clear))

    allfilter_clear = initfilter_clear
    
    countTest = 0L
    ; Loop thru. surface type to plot
    FOR isfctype = 0, 0 DO BEGIN
      ; Loop thru. channels to plot
      FOR ichan=0,nchan-1 DO BEGIN
        ; check for good data
        filter_Alln = WHERE(refRadObs.lat[initfilter_clear] GE MIN_LAT           $
                           AND refRadObs.lat[initfilter_clear] LE MAX_LAT                $
                           AND refRadObs.tb(initfilter_clear,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadObs.tb(initfilter_clear,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.lat[initfilter_clear] GE MIN_LAT                $
                           AND refRadSim.lat[initfilter_clear] LE MAX_LAT                $
                           AND refRadSim.tb(initfilter_clear,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                           AND refRadSim.tb(initfilter_clear,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                           AND sfctype[initfilter_clear] eq isfctype, nAlln )
        ; no good data continue 
        IF (nAlln eq 0) THEN continue 
        x1 = REFORM(refRadObs.tbdiff[initfilter_clear[filter_Alln],chPlotArr[ichan]])
        cnt = n_elements(x1)
        res1=histogram(x1,nbins=nbin,locations=loc1)
        res2=res1
        res1=res1/float(max(res1))*100.
        ind=where(res1 ge 0.)
        M1=mean(x1)
        if (M1 gt -0.000199 and M1 lt 0.000199) then M1=0.0
        S1=stdev(x1)

        mxr = MAX(res1,ipk)

        histPeak[ichan,isfctype] = loc1[ipk]
        ;---Fill Bias and Stdv arrays for xyouts
        histBias[ichan,isfctype] = M1
        histStdv[ichan,isfctype] = S1

        ;---center the distribution about the peak of the PDF
        x2 = x1 - loc1[ipk[0]]
        z2 = x2 
        ;---parse user options.
        ; if trimOption gt 0, then scale the
        ; standard deviation of the unfiltered distribution
        ; otherwise, use the value in trimValue
        trim = S1
        IF (trimOption[ichan] eq 0) THEN trim = MAX(ABS(x2))  ; no trimming
        IF (trimOption[ichan] ne 0) THEN BEGIN
           IF (trimOption[ichan] gt 0) THEN $
              trim = trim*FLOAT(trimOption[ichan]) ELSE $
                 trim = trimValue[ichan]
        ENDIF
        i_trim = WHERE(ABS(z2) le trim,ni_trim)
        x2 = x2[i_trim]
        cnt2 = n_elements(x2)
        res2=histogram(x2,nbins=nbin,locations=loc2)
        res3=res2
        res2=res2/float(max(res2))*100.
        ind=where(res2 ge 0.)
        M2 = mean(x2)
        S2 = STDDEV(x2)
        IF (useforclear[iChan] ne 0) THEN BEGIN
          IF (countTest eq 0) THEN i_clear2 = initfilter_clear[filter_Alln[i_trim]] ELSE $
             i_clear2 = INTERSECT(i_clear2,initfilter_clear[filter_Alln[i_trim]])
          countTest += 1  ; add to counter
        ENDIF
        
;        help, i_clear, i_clear2, ifilter_all[filter_Alln]
        ;---only plot histograms over ocean
        IF (isfctype ne 0) THEN continue 
        ;---Plot axis min/max
        ymin1=min(res1)
        ymax1=max(res1)
        ymin=0
        ymax=ymax1
        if (min(x1) lt -4 or max(x1) gt 4)  then xmin=-10
        if (min(x1) lt -4 or max(x1) gt 4)  then xmax=10
        if (min(x1) ge -4 and max(x1) le 4) then xmin=-5
        if (min(x1) ge -4 or max(x1) le 4)  then xmax=5
        if (min(x1) lt -10 or max(x1) gt 10)  then xmin=-15
        if (min(x1) lt -10 or max(x1) gt 10)  then xmax=15

        xtit='dTb (K)'
        ytit='Peak Normalized Number of Pts'
        dumx=[1,2] & dumy=[1,2]
        channel = 'ch' + chanNumArr[chPlotArr(iChan)]   $
                  +  ' (' + chanInfoArr[chPlotArr(iChan)] + freq_unit +') '
        title = sensorName + ' dTb ' + channel + date

        PLOT, dumx, dumy, CHARSIZE=1, TITLE=title,$
          COLOR=0, BACKGROUND=255, XRANGE = [xmin, xmax], YRANGE=[ymin,ymax],      $
          ytitle=ytit, xtitle=xtit, xstyle=1,ystyle=1,/NODATA
        
        oplot,loc1,res1,color=240,linestyle=0,thick=3
        oplot,loc2,res2,color=240,linestyle=1,thick=3
        plots,[M1,M1],[ymin,ymax],color=240,linestyle=0,thick=1
        plots,[0,0],[0,100],thick=3,color=10
        
        comm1=strmid(strcompress('N Obs: ' + string(cnt)),0,16)
        if (histBias[ichan] lt 0) then comm2=strmid(strcompress('Bias: ' + string(histBias[ichan])),0,13)
        if (histBias[ichan] ge 0) then comm2=strmid(strcompress('Bias: ' + string(histBias[ichan])),0,12)
        comm3=strmid(strcompress('Stdv: ' + string(histStdv[ichan])),0,12)

        comm4=STRCOMPRESS(STRTRIM('N Obs_trim: ' + string(cnt2),2))
        comm5=STRTRIM('Bias_trim: ' + string(M2,"(f7.4)"),2)
        comm6=STRTRIM('Stdv_trim: ' + string(S2,"(f7.4)"),2)
        
        if (xmin eq -10) then xs=-8.5
        if (xmin eq -15) then xs=-12.5
        if (xmin eq -5)  then xs=-4.5
        r0=ymin
        r=ymax
        sfcstr = 'ocean'
        xyouts,xs,r0+(0.93*(r-r0)),sfcstr,charsize=0.75
        xyouts,xs,r0+(0.87*(r-r0)),comm1,charsize=0.75
        xyouts,xs,r0+(0.81*(r-r0)),comm2,charsize=0.75
        xyouts,xs,r0+(0.75*(r-r0)),comm3,charsize=0.75

        xyouts,xs,r0+(0.69*(r-r0)),comm4,charsize=0.75
        xyouts,xs,r0+(0.63*(r-r0)),comm5,charsize=0.75
        xyouts,xs,r0+(0.57*(r-r0)),comm6,charsize=0.75
        IF (iChan ne 0 and (iChan+1 MOD 4) eq 0) THEN ERASE
      ENDFOR
      DEVICE, /close
    ENDFOR

    ;---if user wanted histogram adjust
    ;determined "clear" pass back indices
    ;otherwise, return initial filter
    IF (countTest eq 0) THEN filter_clear = initfilter_clear ELSE filter_clear = i_clear2

END
