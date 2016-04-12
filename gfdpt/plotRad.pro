;---------------------------------------------------------------------------------
; Name:  plotRad.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot both observed radiance and simulated radiance.
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 5, 2014, DXu, Initial coding
;          June 18, 2014, ESM, modified map plotting symbols,
;          fontsize, output postscript size, 
;          plot gridded data instead of raw data 
;
;---------------------------------------------------------------------------------
PRO plotRad, chPlotArr, inputConfig, refRadObs, refRadSim
    ; Get input config params
    chanNumArr = inputConfig.chanNumArr
    chanInfoArr = inputConfig.chanInfoArr
    prefix = inputConfig.prefix
    MIN_LAT = inputConfig.MIN_LAT
    MAX_LAT = inputConfig.MAX_LAT
    MIN_LON = inputConfig.MIN_LON
    MAX_LON = inputConfig.MAX_LON
    minBT_Values = inputConfig.minBT_Values
    maxBT_Values = inputConfig.maxBT_Values
    date = inputConfig.date
    nodeFlag = inputConfig.nodeFlag
    freq_unit = inputConfig.freq_unit
   ; Get sensor name 
   sensorName = inputConfig.sensorName
   
   ; Set XSIZE and YSIZE for PS.
   xSizeVal=18
   ySizeVal=22

   ; Save graphics in PS
   SET_PLOT, 'PS'

   ; Get num of channels to plot
   numOfChans = N_ELEMENTS(chPlotArr)

   ; Number of channels per page
   PLOT_PER_PAGE = 4

   ; Total number of graphics files
   IF ((numOfChans MOD PLOT_PER_PAGE) eq 0 ) THEN BEGIN
      nFiles = numOfChans / PLOT_PER_PAGE
   ENDIF ELSE BEGIN
      nFiles = numOfChans / PLOT_PER_PAGE + 1
   ENDELSE
   ; Create string array with an extra element, so we
   ; can refer to it as 1-base string array.
   fileNumArr = SINDGEN(nFiles+1)
   ; Graphics file number tracking: 1-base
   fileIndex = 1

   ;; findTB_DiffRange, MIN_LAT, MAX_LAT, MIN_LON, MAX_LON, $
   ;;    chPlotArr, refRadObs, refRadSim,   $
   ;;    minBT_DiffArr, maxBT_DiffArr
   minBT_DiffArr = FLTARR(N_ELEMENTS(chPlotArr))-30
   maxBT_DiffArr = FLTARR(N_ELEMENTS(chPlotArr))+30
   ; Loop thru. channels to plot
   FOR iChan=0, numOfChans - 1 DO BEGIN
      ;esm only open file if valid data 
      rowPosition = iChan MOD PLOT_PER_PAGE
;      print, ichan, rowPosition
      IF ( rowPosition eq 0 ) THEN BEGIN
         tmpstr = ''
         IF (nodeFlag EQ 0) THEN tmpStr = '_asc'
         IF (nodeFlag EQ 1) THEN tmpStr = '_desc'
         imageName = STRCOMPRESS(prefix + fileNumArr(fileIndex) + tmpStr + '.eps',/remove_all)
         ; Get the file number for the next graphics file
         fileIndex = fileIndex + 1
         LOADCT, 39
         !P.FONT=0
         DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
                 XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=1, YOFFSET=1,  $
                 /PORTRAIT, FONT_SIZE=5, /BOLD, /COURIER
      ENDIF
      ;-----------------
      ; Define filter
      ;-----------------
      ;----------------------------
      ;  Filter for all orbits
      ;----------------------------
      ;filter_All = WHERE(refRadObs.lat GE MIN_LAT           $
      ;          AND refRadObs.lat LE MAX_LAT                $
      ;          AND refRadObs.tb(*,chPlotArr(iChan)) GT 0 $
      ;          AND refRadSim.lat GE MIN_LAT                $
      ;          AND refRadSim.lat LE MAX_LAT                $
      ;          AND refRadSim.tb(*,chPlotArr(iChan)) GT 0 )

      ;----------------------------
      ;  Filter for ascending orbits
      ;----------------------------
      IF (nodeFlag EQ 0) THEN BEGIN
	 filter_All = WHERE(refRadObs.lat GE MIN_LAT           $
		   AND refRadObs.lat LE MAX_LAT                $
                   AND refRadObs.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                   AND refRadObs.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
		   AND refRadObs.nodeFlag EQ 0                 $
		   AND refRadSim.lat GE MIN_LAT                $
		   AND refRadSim.lat LE MAX_LAT                $
		   AND refRadSim.nodeFlag EQ 0 $
		   AND refRadSim.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
		   AND refRadSim.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)],  nAll )
      endif 
      ;----------------------------
      ;  Filter for descending orbits
      ;----------------------------
      IF (nodeFlag EQ 1) THEN BEGIN
	 filter_All = WHERE(refRadObs.lat GE MIN_LAT          $
		   AND refRadObs.lat LE MAX_LAT                $
		   AND refRadObs.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
		   AND refRadObs.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
		   AND refRadObs.nodeFlag EQ 1                 $
		   AND refRadSim.lat GE MIN_LAT                $
		   AND refRadSim.lat LE MAX_LAT                $
		   AND refRadSim.nodeFlag EQ 1 $
		   AND refRadSim.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
		   AND refRadSim.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)], nAll )
      ENDIF 
      if (nAll eq 0) then continue 
      ; Start a new page every PLOT_PER_PAGE(8) channels. 
      ;------------------------------------------------
      ; step 1:
      ;   Plot observed radiances for chosen channels.
      ;------------------------------------------------
      channel = 'ch' + chanNumArr[chPlotArr(iChan)]   $
                +  ' (' + chanInfoArr[chPlotArr(iChan)] + ' ' + freq_unit +') '
      title = sensorName + ' obs Tb ' + channel + date


      ; Generate plot position
      ; Column position
      colPosition = 0
      
      ; Get min and max of observed and simulated radiances for each channel. 
      minBT = min( [refRadObs.tb(filter_All,chPlotArr(iChan)),   $ 
                    refRadSim.tb(filter_All,chPlotArr(iChan))] )
      maxBT = max( [refRadObs.tb(filter_All,chPlotArr(iChan)),   $
                    refRadSim.tb(filter_All,chPlotArr(iChan))] )
      ; Round to multiples of 10
      minBT = FIX(minBT/10)*10
      maxBT = FIX(maxBT/10)*10 + 10
      dlatlon = 0.5
      ; grid data for map plotting --esm
      GRID_FOR_MAP, refRadObs.tb(filter_All, chPlotArr(iChan)), $
                    refRadSim.tb(filter_All, chPlotArr(iChan)), $
                    refRadObs.lon(filter_All), $
                    refRadObs.lat(filter_All), MIN_LON, MAX_LON, MIN_LAT, MAX_LAT,  $
                    dlatlon, rgridobs, rgridsim, latgrid, longrid, filter_Allg
      
      ; Plot observed TB
      radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,   $
	       latgrid,longrid,                  $
	       filter_Allg,   $
	       title,     $
	       minBT, maxBT, $
	       rgridobs, $
	       'K', $   ;unit
	       0.8, $   ;scale
	       3,   $   ;symb
	       1,   $   ;thick
	       '(i5)', $ ;fmt
               rowPosition, colPosition
      
      ;-------------------------------------------------
      ; step 2:
      ;   Plot simulated radiances for chosen channels.
      ;-------------------------------------------------
      title = sensorName + ' sim Tb ' + channel + date

      ; Column position
      colPosition = 1

      radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,   $
	       latgrid,longrid,                  $
	       filter_Allg,   $
	       title,     $
	       minBT, maxBT, $
	       rgridsim, $
	       'K', $   ;unit
	       0.8, $   ;scale
	       3,   $   ;symb
	       1,   $   ;thick
	       '(i5)', $ ;fmt
               rowPosition, colPosition

      ;-------------------------------------------------
      ; step 3:
      ;   Plot radiance difference 
      ;-------------------------------------------------
      title = sensorName + ' DTb (O-B) ' + channel + date

      ; Column position
      colPosition = 2

      ; Get min and max of radiance differences for each channel 
      ; (indexing 0:nChan-1 in findTB_DiffRange)
      minTB_DiffValue = minBT_DiffArr(iChan)
      maxTB_DiffValue = maxBT_DiffArr(iChan)

      filter_Alln = WHERE(refRadObs.lat GE MIN_LAT           $
                          AND refRadObs.lat LE MAX_LAT                $
                          AND refRadObs.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                          AND refRadObs.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)] $
                          AND refRadSim.lat GE MIN_LAT                $
                          AND refRadSim.lat LE MAX_LAT                $
                          AND refRadSim.tb(*,chPlotArr(iChan)) GT minBT_Values[chPlotArr(ichan)] $
                          AND refRadSim.tb(*,chPlotArr(iChan)) LT maxBT_Values[chPlotArr(ichan)], nAlln )
      IF (nAlln eq 0) THEN continue ; plot figure without data?
      ; max/min plot set via max(abs(tbDiff)) for asc and desc.
      maxTB_DiffValue = $
         MAX(ABS(refRadObs.tbDiff(filter_Alln, chPlotArr(iChan))))
      minTB_DiffValue = $
         -1.*MAX(ABS(refRadObs.tbDiff(filter_Alln, chPlotArr(iChan))))
      TB_DiffValue = PERCENTILE_RANK($
                     (refRadObs.tbDiff(filter_Alln, chPlotArr(iChan))),[2.,98.])
      minTB_DiffValue = FLOAT(FLOOR(TB_DiffValue[0]))
      maxTB_DiffValue = FLOAT(CEIL(TB_DiffValue[1]))
      iposAM = STRPOS(sensorName,'AMSU')
      iposAM = iposAM + STRPOS(sensorName,'ATMS')
      iposAM = iposAM + STRPOS(sensorName,'AMSR')
      IF (STRMID(sensorName,0,3) eq 'GPM' or $
          STRMID(sensorName,0,3) eq 'F18' or $
          STRMID(sensorName,0,2) eq 'MT' or $
          iposAM ge 0) THEN BEGIN
        minTB_DiffValue = -10.
        maxTB_DiffValue = 10.
      ENDIF
      print, minTB_DiffValue, maxTB_DiffValue
      
      radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,   $
	       latgrid,longrid,                  $
	       filter_Allg,   $
	       title,     $
	       minTB_DiffValue, maxTB_DiffValue, $
	       rgridobs-rgridsim, $
	       'K', $   ; unit
	       0.8, $   ; scale
	       3,   $   ; symb
	       1,   $   ; thick
	       '(f7.2)', $ ; fmt
               rowPosition, colPosition

      ; Close 'PS' file every PLOT_PER_PAGE (4)
      IF (rowPosition EQ PLOT_PER_PAGE - 1 ||   $
          iChan eq numOfChans - 1 ) THEN BEGIN
         DEVICE, /CLOSE
         ;cgPS2Raster, imageName, /PNG, Width=900, portrait=1
         ;File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
      ENDIF 

;      radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,   $
;	       refRadObs.lat,refRadObs.lon,                  $
;	       filter_All,   $
;	       title,     $
;	       minBT, maxBT, $
;	       refRadObs.tb(*, chPlotArr(iChan)), $
;	       'K', $   ;unit
;	       0.8, $   ;scale
;	       8,   $   ;symb
;	       1,   $   ;thick
;	       '(i5)', $ ;fmt
;              rowPosition, colPosition
      ; Plot simulated TB
      ;; radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
      ;;          refRadSim.lat,refRadSim.lon,                  $
      ;;          filter_All,   $
      ;;          title,     $
      ;;          minBT, maxBT, $
      ;;          refRadSim.tb(*, chPlotArr(iChan)),         $
      ;;          'K', $   ;unit
      ;;          0.8, $   ;scale
      ;;          8,   $   ;symb
      ;;          1,   $   ;thick
      ;;          '(i5)', $ ;fmt
      ;;          rowPosition, colPosition
      ;; radPloting, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
      ;;          refRadObs.lat,refRadObs.lon,    $
      ;;          filter_All,   $
      ;;          title,     $
      ;;          minTB_DiffValue,    $
      ;;          maxTB_DiffValue,    $
      ;;          refRadObs.tbDiff(*, chPlotArr(iChan)), $
      ;;          'K', $   ;unit
      ;;          0.8, $   ;scale
      ;;          8,   $   ;symb
      ;;          1,   $   ;thick
      ;;          '(f5.1)', $ ;fmt
      ;;          rowPosition, colPosition

   ENDFOR

END

;======================================================

PRO radPloting, MIN_LAT, MAX_LAT, MIN_LON, MAX_LON,   $
    latVect, lonVect, filter, title,                  $
    minValue, maxValue, data, unit, scal,             $ 
    symb, thickVal, fmt, rowPosition, colPosition

   ; set the default charsz
   charSize=1.
   nRec=n_elements(filter)
   
   ; num of colos 
   nColor=!D.table_size
   nColor=nColor-2

   csize=16
   ; Divide 2.5PI by 16.
   tmpVal = findgen(csize+1) * (!PI*2.5/float(csize))
   usersym,scal*cos(tmpVal)/2,scal*sin(tmpVal)/2,/fill

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   center_lon=0
   latdelVal=max([fix((MAX_LAT-MIN_LAT)/100),1])*30
   londelVal=max([fix((MAX_LON-MIN_LON)/100),1])*20

   ; Define where plots start
   xOrigin = 0.02
   yOrigin = 0.03

   ; plot width
   plotWidth = 0.31
   ; plot height
   plotHeight = 0.17
   ; color bar height
   barHeight = 0.02

   xSpacer = 0.02  ; space between two plots horizontally.
   ySpacer = 0.04  ; space between bar and next plot vertically.
   innerSpacer = 0.01    ; space between plot and bar vertically.

   plotY_Pos = findgen(4) ; array holding box position
   barY_Pos = findgen(4)  ; array holding bar position

   ; Define y positions of boxes and bars
   FOR i=0, 3 DO BEGIN
      ; box's y positions
      plotY_Pos(i) = yOrigin + barHeight + innerSpacer    $
	      + ( 3 - i ) * (plotHeight + innerSpacer + barHeight + ySpacer )
      ; bar's y positions
      barY_Pos(i) = yOrigin + ( 3 - i )     $
              * (plotHeight + innerSpacer + barHeight + ySpacer )
   ENDFOR

   ; Define box's position
   IF ( colPosition eq 0 ) THEN BEGIN             ; left
      x0 = xOrigin
      y0 = plotY_Pos ( rowPosition )
      x1 = xOrigin + plotWidth
      y1 = plotY_Pos ( rowPosition ) + plotHeight
   ENDIF ELSE IF ( colPosition eq 1 ) THEN BEGIN  ; middle
      x0 = xOrigin + ( plotWidth + xSpacer )
      y0 = plotY_Pos ( rowPosition )
      x1 = xOrigin + ( plotWidth + xSpacer ) + plotWidth
      y1 = plotY_Pos ( rowPosition ) + plotHeight
   ENDIF ELSE BEGIN                               ; right
      x0 = xOrigin + 2 * ( plotWidth + xSpacer )
      y0 = plotY_Pos ( rowPosition )
      x1 = xOrigin + 2 * ( plotWidth + xSpacer ) + plotWidth
      y1 = plotY_Pos ( rowPosition ) + plotHeight
   ENDELSE

   ; Cylindrical is the default map projection.
   ; /LABEL is to draw parallels and mereidans 
   ; /NOERASE is to NOT erase current window so we can have multiple maps drawn on 
   ; the same page.
   MAP_SET, 0,0, CHARSIZE=1, /LABEL, LATLAB = MIN_LON, LONLAB = MIN_LAT, LATDEL=latdelVal, $ 
      LONDEL=londelVal, POSITION=[x0,y0,x1,y1], /NOERASE, TITLE=title
   map_plot_contour = 0
   IF (map_plot_contour eq 0) THEN BEGIN
     FOR iProf=0L,nRec-1 DO BEGIN
      ; Get index value
      index = filter(iProf)
      ; Get data value
      dataVal=data[index]
      colorVal=0L
      colorNum=(float(dataVal-minValue)/float(maxValue-minValue))*nColor
      ; Final value will be in range (1, nColor)
      colorVal=long(colorNum) > 1L < nColor

      OPLOT,[lonVect[index]],[latVect[index]],color=colorVal-1,psym=symb,$
         symsize=scal,thick=thickVal

     ENDFOR
   ENDIF ELSE BEGIN  
      ;---do not use ... --esm 140618
      levs = minValue + (maxValue-minValue)*findgen(100)/99.
      cols = (levs - minValue)/FLOAT(maxValue - minValue)*nColor
      cols = LONG(cols) > 1L < nColor
      datac = REFORM(data[filter])
      lats  = REFORM(latVect[filter])
      lons  = REFORM(lonVect[filter])
      
      CONTOUR,datac,lons,lats,levels=levs,c_colors=cols,$
              position=[x0,y0,x1,y1], /cell_fill, /overplot, /irregular 
   ENDELSE   
   ; Plot continent after plotting data points to overplot data
   MAP_CONTINENTS, /HIRES, FILL_CONTINENTS=0,  MLINESTYLE=0, MLINETHICK=1, COLOR=0

   IF ((maxValue-minValue) gt 0.) THEN BEGIN
      ; Define the position of color bar
      IF ( colPosition eq 0 ) THEN BEGIN 
	 x0 = xOrigin
	 y0 = barY_Pos ( rowPosition )
	 x1 = xOrigin + plotWidth
	 y1 = barY_Pos ( rowPosition ) + barHeight
      ENDIF ELSE IF ( colPosition eq 1 ) THEN BEGIN
	 x0 = xOrigin + ( plotWidth + xSpacer ) 
	 y0 = barY_Pos ( rowPosition )
	 x1 = xOrigin + ( plotWidth + xSpacer ) + plotWidth
	 y1 = barY_Pos ( rowPosition ) + barHeight
      ENDIF ELSE BEGIN
	 x0 = xOrigin + 2 * ( plotWidth + xSpacer )
	 y0 = barY_Pos ( rowPosition )
	 x1 = xOrigin + 2 * ( plotWidth + xSpacer ) + plotWidth
	 y1 = barY_Pos ( rowPosition ) + barHeight
      ENDELSE 

      COLORBAR,NCOLORS=nColor,/HORIZONTAL,RANGE=[minValue,maxValue],TITLE=unit,$
	  FORMAT=fmt,CHARSIZE=charSize*1.1,FONT=1,POSITION=[x0, y0, x1, y1]
   ENDIF ELSE BEGIN
      PRINT, minValue, maxValue
      PRINT, 'Warning: in radPloting: No color bar plotted: (maxValue-minValue) <= 0.'
   ENDELSE
END

PRO findTB_DiffRange, MIN_LAT, MAX_LAT, MIN_LON, MAX_LON,   $
   chPlotArr, refRadObs, refRadSim, $
   minBT_DiffArr, maxBT_DiffArr
   ;
   ; Loop thru all the channels to find the shared max and min of tb diff, 
   ; which will be used to plot tb diff for all the channels.
   ; However, if max/min for a specific channel is too large, then this channel is 
   ; excluded in finding shared max/min and we will use its own max/min to plot 
   ; tb diff for that channel.
   ;
   extreme=30
   minTB_DiffValue = 0.0
   maxTB_DiffValue = 0.0
   numOfChans = n_elements(chPlotArr)
   
   ; Create arrays to hold min/max of BT diff for channels to plot
   FILL_VALUE = 999.99
   minBT_DiffArr = make_array(numOfChans, /float, value=FILL_VALUE)
   maxBT_DiffArr = make_array(numOfChans, /float, value=FILL_VALUE)

   ; Define a rannge of bt difference we think 
   ; that represents min/max of bt diffs for all channels.
   ; We can get it from common config params later.
   LOW_BT_DIFF = -30        ; lowest bt diff
   HIGH_BT_DIFF = 30        ; highest bt diff
   ratioThreshold = 0.01    ; 1% of data are extreme

   FOR iChan=0, numOfChans - 1 DO BEGIN
      filter_All = WHERE(refRadObs.lat GE MIN_LAT           $
                AND refRadObs.lat LE MAX_LAT                $
                AND refRadObs.tb(*,chPlotArr(iChan)) GT 0 $
                AND refRadSim.lat GE MIN_LAT                $
                AND refRadSim.lat LE MAX_LAT                $
                AND refRadSim.tb(*,chPlotArr(iChan)) GT 0 )

      ; Valid number of points
      numValid = n_elements(filter_all) 

      ; Find min of bt diff for a channel
      tmpValue = min(refRadObs.tbDiff(filter_All, chPlotArr(iChan)))

      ; 
      ; Use it to find commmon min if it's within range. 
      ; 
      IF ( tmpValue GT LOW_BT_DIFF ) THEN BEGIN
	 minTB_DiffValue = ( minTB_DiffValue LT tmpValue ) ? minTB_DiffValue : tmpValue  
      ENDIF ELSE BEGIN
	 extLowCounter = 0  
         ; Count the number of points that are beyond min range
	 FOR i = 0, numValid-1  DO BEGIN 
           ; Get the bt diff value
	   tmpValue2 = refRadObs.tbDiff(filter_All[i], chPlotArr(iChan))
	   IF ( tmpValue2 LE LOW_BT_DIFF ) THEN extLowCounter++
	 ENDFOR
         
         ; convert to float by times 1.0 
         ratio = 1.0 * extLowCounter / numValid  
         ; Round to multiples of 10
         IF ( ratio GT ratioThreshold ) THEN minBT_DiffArr(iChan) = FIX(tmpValue/10)*10
      ENDELSE

      ; Find max of bt diff for a channel
      tmpValue = max(refRadObs.tbDiff(filter_All, chPlotArr(iChan)))

      ; 
      ; Use it to find commmon max if it's within range. 
      ; 
      IF ( tmpValue LT HIGH_BT_DIFF ) THEN BEGIN
	 maxTB_DiffValue = ( maxTB_DiffValue GT tmpValue ) ? maxTB_DiffValue : tmpValue  
      ENDIF ELSE BEGIN
	 extHighCounter = 0  
         ; Count the number of points that are beyond max range
	 FOR i = 0, numValid-1  DO BEGIN 
           ; Get the bt diff value
	   tmpValue2 = refRadObs.tbDiff(filter_All[i], chPlotArr(iChan))
	   IF ( tmpValue2 GE HIGH_BT_DIFF ) THEN extHighCounter++
	 ENDFOR
        
         ; convert to float by times 1.0 
         ratio = 1.0 * extHighCounter / numValid 
         ; Round to multiples of 10
         IF ( ratio GT ratioThreshold ) THEN maxBT_DiffArr(iChan) = FIX(tmpValue/10) * 10 + 10
      ENDELSE
   ENDFOR

   FOR iChan=0, numOfChans - 1 DO BEGIN
      ; Use real common min/max
      ;IF (minBT_DiffArr(iChan) EQ FILL_VALUE) THEN minBT_DiffArr(iChan) = minTB_DiffValue
      ;IF (maxBT_DiffArr(iChan) EQ FILL_VALUE) THEN maxBT_DiffArr(iChan) = maxTB_DiffValue
      ; Use defined min/max value configured 
      IF (minBT_DiffArr(iChan) EQ FILL_VALUE) THEN minBT_DiffArr(iChan) = LOW_BT_DIFF
      IF (maxBT_DiffArr(iChan) EQ FILL_VALUE) THEN maxBT_DiffArr(iChan) = HIGH_BT_DIFF
   ENDFOR
   
END 
