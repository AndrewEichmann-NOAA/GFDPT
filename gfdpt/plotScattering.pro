;---------------------------------------------------------------------------------
; Name:  plotScatter.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot scatter among parameters 
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Mar 21, 2014, DXu, Initial coding
;          Jun 18, 2014, ESM, various modifications 
;          Oct 20, 2014, MoharC, Moisture componenets and changed
;          yrange in drawDTb_Density
;
;---------------------------------------------------------------------------------
PRO plotScattering, chPlotArr, inputConfig, refRadObs, refRadSim, refSceneData
    ; Get input config parameters
    chanNumArr = inputConfig.chanNumArr
    chanInfoArr = inputConfig.chanInfoArr
    maxDTb_Values = inputConfig.maxDTb_Values
    prefix = inputConfig.prefix
    
    sensorName = inputConfig.sensorName
    MIN_LAT = inputConfig.MIN_LAT
    MAX_LAT = inputConfig.MAX_LAT
    MIN_LON = inputConfig.MIN_LON
    MAX_LON = inputConfig.MAX_LON
    date = inputConfig.date
    freq_unit  = inputConfig.freq_unit

   ; Save graphics in PS
   SET_PLOT, 'PS'

   ; Get num of channels to plot
   numOfChans = N_ELEMENTS(chPlotArr)
   minBT_Values = inputConfig.minBT_Values
   maxBT_Values = inputConfig.maxBT_Values

   ; Loop thru. channels to plot
   FOR iChan=0, numOfChans - 1 DO BEGIN
      ; chan no (integer)
      chanNo = chPlotArr(iChan)
      ; chan no (string)
      chanNo_Str = chanNumArr(chanNo) 
      ; Channel frequency info
      chanInfo =  chanInfoArr(chanNo)
      ; Sfc type 0 : Ocean
      f1 = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refSceneData.SfcTypeVec EQ 0 ,nf1)
      ; Sfc type 1 : Sea-Ice
      f2 = WHERE(refRadObs.lat GE MIN_LAT                $
	    AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
	    AND refRadSim.lat GE MIN_LAT                $
	    AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
	    AND refSceneData.SfcTypeVec EQ 1 ,nf2)
      ; Sfc type 2 : Land
      f3 = WHERE(refRadObs.lat GE MIN_LAT                $
	    AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
	    AND refRadSim.lat GE MIN_LAT                $
	    AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
	    AND refSceneData.SfcTypeVec eq 2 ,nf3)
      ; Sfc type 3 : Snow-covered Land (Snow)
      f4 = WHERE(refRadObs.lat GE MIN_LAT                $
	    AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
	    AND refRadSim.lat GE MIN_LAT                $
	    AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
	    AND refSceneData.SfcTypeVec EQ 3 ,nf4)

      ; Sfc type 0 : Ocean Ascending
      f1a = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 0 $
            AND refSceneData.SfcTypeVec EQ 0 ,nf1a)
      ; Sfc type 1 : Sea-Ice Ascending
      f2a = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 0 $
            AND refSceneData.SfcTypeVec EQ 1 ,nf2a)
      ; Sfc type 2 : Land Ascending
      f3a = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 0 $
            AND refSceneData.SfcTypeVec eq 2 ,nf3a)
      ; Sfc type 3 : Snow-covered Land (Snow) Ascending
      f4a = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 0 $
            AND refSceneData.SfcTypeVec EQ 3 ,nf4a)

      ; Sfc type 0 : Ocean Descending
      f1d = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 1 $
            AND refSceneData.SfcTypeVec EQ 0 ,nf1d) 
      ; Sfc type 1 : Sea-Ice Descending
      f2d = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 1 $
            AND refSceneData.SfcTypeVec EQ 1 ,nf2d)
      ; Sfc type 2 : Land Descending
      f3d = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 1 $
            AND refSceneData.SfcTypeVec eq 2 ,nf3d)
      ; Sfc type 3 : Snow-covered Land (Snow) Descending
      f4d = WHERE(refRadObs.lat GE MIN_LAT                $
            AND refRadObs.lat LE MAX_LAT                $
            AND refRadObs.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadObs.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadSim.lat GE MIN_LAT                $
            AND refRadSim.lat LE MAX_LAT                $
            AND refRadSim.tb(*, chanNo) GT minBT_Values[ichan] $
            AND refRadSim.tb(*, chanNo) LT maxBT_Values[ichan] $
            AND refRadObs.nodeFlag EQ 1 $
            AND refSceneData.SfcTypeVec EQ 3 ,nf4d)

      ; Sfc type name 
      sfcTypeNames = ['Sfc Type: Ocean', 'Sfc Type: Sea-Ice',  $
                      'Sfc Type: Land', 'Sfc Type: Snow' ]

      ; Max DTb
      maxDTb = maxDTb_Values(chanNo)

      dirname = FILE_DIRNAME(prefix)
      ; Create a temp data struture to simplify interface
      confObj = { $  
         prefix : prefix,$
; dirname + '/' + sensorName + '_BiasPlot',$
	 chanNo : chanNo,$
         chanNo_Str : chanNo_Str,$
	 chanInfo : chanInfo,$
	 date : date,$
	 maxDTb : maxDTb,$
	 sensorName : sensorName,$
	 MIN_LAT : MIN_LAT,$
	 MAX_LAT : MAX_LAT,$
	 MIN_LON : MIN_LON,$
	 MAX_LON : MAX_LON $
      }

      filterObj = { $
                  f1 : f1, f2 : f2, f3 : f3, f4 : f4,$
                  f1a : f1a, f2a : f2a, f3a : f3a, f4a : f4a,$
                  f1d : f1d, f2d : f2d, f3d : f3d, f4d : f4d, $
                  nf1 : nf1, nf2 : nf2, nf3 : nf3, nf4 : nf4,$
                  nf1a : nf1a, nf2a : nf2a, nf3a : nf3a, nf4a : nf4a,$
                  nf1d : nf1d, nf2d : nf2d, nf3d : nf3d, nf4d : nf4d }
     
      ;--------------------
      ; radObs vs radSim
      ;--------------------
      imageNameRad = STRCOMPRESS(prefix + 'Rad_ch' + chanNo_Str + '',/remove_all)
      titleRad = STRCOMPRESS(sensorName + ' radObs vs radSim (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotDenTb, imageNameRad, titleRad,  $
         'sim Tb (k)', 'obs Tb (k)', $
         refRadSim.tb,   $
         refRadObs.tb, f1, f2, f3, f4, $
                 nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames

      ;----------------------------------------------------------
      ; Stratify Bias plots by sfc type, lat, node, etc.
      ;----------------------------------------------------------
;      plotBiasStratified, refRadObs, filterObj, confObj

      ;--------------------
      ; tbDiff vs lat
      ;--------------------
      imageNameLat = STRCOMPRESS(prefix + 'Lat_ch' + chanNo_Str + '',/remove_all)
      titleLat = STRCOMPRESS(sensorName + ' TB diff vs Lat (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'Lat (deg)'
      titleY = 'DTb (O-B) (k)'
      plotDenDTb, imageNameLat, titleLat,  $
                  titleX, titleY,   $
                  refRadObs.lat,   $
                  refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
      ;--------------------
      ; tbDiff vs analytic emis regression
      ;--------------------
      imageNameLat = STRCOMPRESS(prefix + 'emisdiff_ch' + chanNo_Str + '',/remove_all)
      titleLat = STRCOMPRESS(sensorName + ' TB diff vs regression emis (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'emis(regression) '
      titleY = 'DTb (O-B) (k)'
      plotDenDTb, imageNameLat, titleLat,  $
                  titleX, titleY,   $
                  refsceneData.emissanlvec[*,ChanNo],   $
                  refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
      
      ;--------------------
      ; tbDiff vs TPW
      ;--------------------
      imageNameTPW = STRCOMPRESS(prefix + 'TPW_ch' + chanNo_Str + '',/remove_all)
      titleTPW = STRCOMPRESS(sensorName + ' TB diff vs TPW (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'TPW (mm)'
      titleY = 'DTb (O-B) (k)'
      plotDenDTb, imageNameTPW, titleTPW, $
         titleX, titleY,   $
         refSceneData.tpwVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)

      ;--------------------
      ; tbDiff vs SkinT
      ;--------------------
      imageNameSkinT = STRCOMPRESS(prefix + 'SkinT_ch' + chanNo_Str + '',/remove_all)
      titleSkinT = STRCOMPRESS(sensorName + ' TB diff vs SkinT (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'SkinT (K)'
      titleY = 'DTb (O-B) (k)'
      plotDenDTb, imageNameSkinT, titleSkinT,   $
         titleX, titleY,   $
         refSceneData.tSkinVec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)

      ;--------------------
      ; tbDiff vs RWP
      ;--------------------
      imageNameSkinT = STRCOMPRESS(prefix + 'RWP_ch' + chanNo_Str + '',/remove_all)
      titleSkinT = STRCOMPRESS(sensorName + ' TB diff vs RWP (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'RWP (mm)'
      titleY = 'DTb (O-B) (k)'
      ;print, "MXC WV", refSceneData.clwVec
      plotDenDTb, imageNameSkinT, titleSkinT,   $
         titleX, titleY,   $
         refSceneData.clwVec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)

      ;--------------------
      ; tbDiff vs GWP
      ;--------------------
      imageNameSkinT = STRCOMPRESS(prefix + 'GWP_ch' + chanNo_Str + '',/remove_all)
      titleSkinT = STRCOMPRESS(sensorName + ' TB diff vs GWP (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'GWP (mm)'
      titleY = 'DTb (O-B) (k)'
      ;print, "MXC WV", refSceneData.clwVec
      plotDenDTb, imageNameSkinT, titleSkinT,   $
         titleX, titleY,   $
         refSceneData.gwpVec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)

      ;--------------------
      ; tbDiff vs WV950hPa
      ;--------------------
      imageNameSkinT = STRCOMPRESS(prefix + 'WV950_ch' + chanNo_Str + '',/remove_all)
      titleSkinT = STRCOMPRESS(sensorName + ' TB diff vs WV950 (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'WV950 (g/kg)'
      titleY = 'DTb (O-B) (k)'
      ;print, "MXC WV", refSceneData.wv950Vec
      plotDenDTb, imageNameSkinT, titleSkinT,   $
         titleX, titleY,   $
         refSceneData.wv950Vec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
      ;--------------------
      ; tbDiff vs WV500hPa
      ;--------------------
      imageNameSkinT = STRCOMPRESS(prefix + 'WV500_ch' + chanNo_Str + '',/remove_all)
      titleSkinT = STRCOMPRESS(sensorName + ' TB diff vs WV500 (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'WV500 (g/kg)'
      titleY = 'DTb (O-B) (k)'
      ;print, "MXC WV", refSceneData.wv500Vec
      plotDenDTb, imageNameSkinT, titleSkinT,   $
         titleX, titleY,   $
         refSceneData.wv500Vec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)

      ;--------------------
      ; tbDiff vs WV300hPa
      ;--------------------
      imageNameSkinT = STRCOMPRESS(prefix + 'WV300_ch' + chanNo_Str + '',/remove_all)
      titleSkinT = STRCOMPRESS(sensorName + ' TB diff vs WV300 (chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      titleX = 'WV300 (g/kg)'
      titleY = 'DTb (O-B) (k)'
      ;print, "MXC WV", refSceneData.wv300Vec
      plotDenDTb, imageNameSkinT, titleSkinT,   $
         titleX, titleY,   $
         refSceneData.wv300Vec,      $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)

 
      ; For conical scanning, we do "DTb vs ScanPos", 
      ; otherwise, we do "DTb vs Angle" 
      ; This list can grow as needed, for now, it's only amsr2 and ssmis
      ; Conical scanning sensor list : 
      ;       GPM_GMI,GCOMW1_AMSR2,F16_SSMIS,F17_SSMIS,F18_SSMIS
      conicalSenArr = ['GPM_GMI', 'GCOMW1_AMSR2','F16_SSMIS','F17_SSMIS','F18_SSMIS','F19_SSMIS']
      ; Assuming sensor is crossing scanning, not conical scanning.
      flag_conical = 0 

      ; Check to see if sensor is one of conical scanners.
      FOR i = 0, N_ELEMENTS(conicalSenArr) - 1 DO BEGIN
	 IF ( sensorName EQ conicalSenArr(i) ) THEN flag_conical = 1
      ENDFOR

      IF ( flag_conical EQ 1 ) THEN BEGIN
	 ;--------------------
	 ; tbDiff vs scanPos
	 ;--------------------
	 imageNameScanPos = STRCOMPRESS(prefix + 'ScanPos_ch' + chanNo_Str + '',/remove_all)
	 titleScanPos = STRCOMPRESS(sensorName + ' TB diff vs scanPos (chan ' + chanNo_Str $
            + ': ' + chanInfo + ") " + date )
         titleX = 'scanPos'
         titleY = 'DTb (O-B) (k)'
	 plotDenDTb, imageNameScanPos, titleScanPos,  $
            titleX, titleY,   $
	    refRadObs.scanPos,   $
	    refRadObs.tbDiff, f1, f2, f3, f4, $
                     nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
      ENDIF ELSE BEGIN
	 ;--------------------
	 ; tbDiff vs angle
	 ;--------------------
	 imageNameAngle = STRCOMPRESS(prefix + 'Angle_ch' + chanNo_Str + '',/remove_all)
	 titleScanPos = STRCOMPRESS(sensorName + ' TB diff vs angle (chan ' + chanNo_Str $ 
            + ': ' + chanInfo + ") " + date )
         titleX = 'angle (deg)'
         titleY = 'DTb (O-B) (k)'
	 plotDenDTb, imageNameAngle, titleAngle,  $
            titleX, titleY,   $
	    refRadObs.angle,   $
	    refRadObs.tbDiff, f1, f2, f3, f4, $
                     nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
      ENDELSE

      continue 
      ;--------------------
      ; tbDiff vs CLW
      ;--------------------
      imageNameCLW = STRCOMPRESS(prefix + 'CLW_ch' + chanNo_Str + '.eps',/remove_all)
      titleCLW = STRCOMPRESS(sensorName + ' TB diff vs CLW (chan ' + chanNo_Str $ 
         + ': ' + chanInfo + ") " + date )
      titleX = 'CLW (mm)'
      titleY = 'DTb (O-B) (k)'
      plotDenDTb, imageNameCLW, titleCLW, $
         titleX, titleY,   $
         refSceneData.clwVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
 
      ;--------------------
      ; tbDiff vs RWP
      ;--------------------
      imageNameRWP = STRCOMPRESS(prefix + 'RWP_ch' + chanNo_Str + '.eps',/remove_all)
      titleRWP = STRCOMPRESS(sensorName + ' TB diff vs RWP (chan ' + chanNo_Str $ 
         + ': ' + chanInfo + ") " + date )
      titleX = 'RWP (mm)'
      titleY = 'DTb (O-B) (k)'
      plotDenDTb, imageNameRWP, titleRWP, $
         titleX, titleY,   $
         refSceneData.rwpVec,  $
         refRadObs.tbDiff, f1, f2, f3, f4, $
                  nf1, nf2, nf3, nf4, ChanNo, sfcTypeNames, maxDTb_Values(ChanNo)
 
      ;--------------------
      ; tbDiff vs GWP
      ;--------------------
      ;imageNameGWP = STRCOMPRESS(prefix + 'GWP_ch' + chanNo_Str + '.eps',/remove_all)
      ;titleGWP = STRCOMPRESS(sensorName + ' TB diff vs GWP (chan ' + chanNo_Str $ 
      ;   + ': ' + chanInfo + ") " + date )
      ;titleX = 'GWP (mm)'
      ;titleY = 'DTb (O-B) (k)'
      ;plotDenDTb, imageNameGWP, titleGWP, $
      ;   titleX, titleY,   $
      ;   refSceneData.gwpVec,  $
      ;   refRadObs.tbDiff, f1, f2, f3, f4, iChan, sfcTypeNames
 

   ENDFOR

END

PRO plotDenDTb, imageName, titleName, titleX, titleY, $
                dataX, dataY, f1, f2, f3, f4, $
                nf1, nf2, nf3, nf4, iChan, sfcTypeNames, tbDiffValue

   LOADCT, 39
   ERASE
   !P.FONT =0
   !P.MULTI = [0, 2, 2]
   aspect_ratio = 0.8 ;rectangle
   xSizeVal = 35
   ySizeVal = xSizeVal * aspect_ratio
   xPosRatio = 0.04  ; 4%
   yPosRatio = 0.04  ; 4% 
   goto, dendtb
   DEVICE, FILENAME=imageName+'.eps', /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
	   /PORTRAIT, FONT_SIZE=11, /BOLD, /COURIER

   if (nf1 ge 2) then begin
     minX = MIN(dataX(f1))
     maxX = MAX(dataX(f1))
     minY = MIN(dataY(f1,iChan))
     maxY = MAX(dataY(f1,iChan))
     posX = minX + xPosRatio * ( maxX - minX )
     posY = minY + (1 - yPosRatio) * ( maxY - minY )
     PLOT, dataX(f1), dataY(f1,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[0], COLOR = 20
                                ;print, 'f1 size ',   N_ELEMENTS(f1)
     generateStats, dataX(f1), dataY(f1,iChan), statArr_1
     plotStats, statArr_1, posX, posY, (maxY - minY)
   endif  else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
     posX = 0 + xPosRatio * ( 1 )
     posY = 1 - yPosRatio * ( 1 )
     XYOUTS, posX, posY, sfcTypeNames[0], COLOR = 20
   endelse
   if (nf2 ge 2) then begin
     minX = MIN(dataX(f2))
     maxX = MAX(dataX(f2))
     minY = MIN(dataY(f2,iChan))
     maxY = MAX(dataY(f2,iChan))
     posX = minX + xPosRatio * ( maxX - minX )
     posY = minY + (1 - yPosRatio) * ( maxY - minY )
     PLOT, dataX(f2), dataY(f2,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[1], COLOR = 20
                                ;print, 'f2 size ',   N_ELEMENTS(f2)
     generateStats, dataX(f2), dataY(f2,iChan), statArr_2
     plotStats, statArr_2, posX, posY, (maxY - minY)
   endif  else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
     posX = 0 + xPosRatio * ( 1 )
     posY = 1 - yPosRatio * ( 1 )
     XYOUTS, posX, posY, sfcTypeNames[1], COLOR = 20
   endelse 
   if (nf3 ge 2) then begin
     minX = MIN(dataX(f3))
     maxX = MAX(dataX(f3))
     minY = MIN(dataY(f3,iChan))
     maxY = MAX(dataY(f3,iChan))
     posX = minX + xPosRatio * ( maxX - minX )
     posY = minY + (1 - yPosRatio) * ( maxY - minY )
     PLOT, dataX(f3), dataY(f3,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[2], COLOR = 20
                                ;print, 'f3 size ',   N_ELEMENTS(f3)
     generateStats, dataX(f3), dataY(f3,iChan), statArr_3
     plotStats, statArr_3, posX, posY, (maxY - minY)
   endif else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
     posX = 0 + xPosRatio * ( 1 )
     posY = 1 - yPosRatio * ( 1 )
     XYOUTS, posX, posY, sfcTypeNames[2], COLOR = 20
   endelse
   if (nf4 ge 2) then begin
     minX = MIN(dataX(f4))
     maxX = MAX(dataX(f4))
     minY = MIN(dataY(f4,iChan))
     maxY = MAX(dataY(f4,iChan))
     posX = minX + xPosRatio * ( maxX - minX )
     posY = maxY - yPosRatio * ( maxY - minY )
     PLOT, dataX(f4), dataY(f4,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[3], COLOR = 20
                                ;print, 'f4 size ',   N_ELEMENTS(f4)
     generateStats, dataX(f4), dataY(f4,iChan), statArr_4
     plotStats, statArr_4, posX, posY, (maxY - minY)
     
   endif else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA

     posX = 0 + xPosRatio * ( 1 )
     posY = 1 - yPosRatio * ( 1 )
     
     XYOUTS, posX, posY, sfcTypeNames[3], COLOR = 20
   endelse
   DEVICE, /CLOSE
   ; Convert to JPEG file and remove PS file
   ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
   ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .eps file
dendtb:
   if (nf1 ge 2) then begin
     x = dataX(f1)
     y = dataY(f1,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_ocean' 
     sfcType = sfcTypeNames[0]
     drawDTb_Density, x, y,  numValid, titleName, titleX,  titleY, $
                      fName, statArr_1, sfcType, tbDiffValue
   endif
   return; skip these other plots they are pretty much useless 
   if (nf2 ge 2) then begin
     x = dataX(f2)
     y = dataY(f2,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_seaice' 
     sfcType = sfcTypeNames[1]
     drawDTb_Density, x, y,  numValid, titleName, titleX,  titleY, $
                      fName, statArr_2, sfcType, tbDiffValue
   endif
   if (nf3 ge 2) then begin
     x = dataX(f3)
     y = dataY(f3,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_land' 
     sfcType = sfcTypeNames[2]
     drawDTb_Density, x, y,  numValid, titleName, titleX,  titleY, $
                      fName, statArr_3, sfcType, tbDiffValue
   endif
   if (nf4 ge 2) then begin
     x = dataX(f4)
     y = dataY(f4,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_snow'
     sfcType = sfcTypeNames[3]
     drawDTb_Density, x, y,  numValid, titleName, titleX,  titleY, $
                      fName, statArr_4, sfcType, tbDiffValue
   endif

END

PRO plotDenTb, imageName, titleName, titleX, titleY, $
               dataX, dataY, f1, f2, f3, f4, $
               nf1, nf2, nf3, nf4, iChan, sfcTypeNames

   LOADCT, 39
   ERASE
   !P.FONT =0
   !P.MULTI = [0, 2, 2]
   goto, dentb
   aspect_ratio = 0.8 ;rectangle
   xSizeVal = 35
   ySizeVal = xSizeVal * aspect_ratio
   xPosRatio = 0.04  ; 4%
   yPosRatio = 0.04  ; 4% 
   DEVICE, FILENAME=imageName+'.eps', /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=2,  $
           /PORTRAIT, FONT_SIZE=11, /BOLD, /COURIER

   if (nf1 ge 2) then begin
     minX = MIN(dataX(f1,iChan))
     maxX = MAX(dataX(f1,iChan))
     minY = MIN(dataY(f1,iChan))
     maxY = MAX(dataY(f1,iChan))
     maxX = MAX([dataX(f1,iChan),dataY(f1,iChan)])
     maxY = MAX([dataX(f1,iChan),dataY(f1,iChan)])
     minX = MIN([dataX(f1,iChan),dataY(f1,iChan)])
     minY = MIN([dataX(f1,iChan),dataY(f1,iChan)])
                                ;print, 'minX, maxX ' , minX, maxX
                                ;print, 'minY, maxY ' , minY, maxY
     posX = minX + xPosRatio * ( maxX - minX )
     posY = minY + (1 - yPosRatio) * ( maxY - minY )
     PLOT, dataX(f1,iChan), dataY(f1,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[0], COLOR = 20
     generateStatsTBvsTB, dataX(f1,iChan), dataY(f1,iChan), statArr_1
     plotStats, statArr_1, posX, posY, (maxY - minY)
   endif else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
   endelse
   if (nf2 ge 2) then begin
     minX = MIN(dataX(f2,iChan))
     maxX = MAX(dataX(f2,iChan))
     minY = MIN(dataY(f2,iChan))
     maxY = MAX(dataY(f2,iChan))
     maxX = MAX([dataX(f2,iChan),dataY(f2,iChan)])
     maxY = MAX([dataX(f2,iChan),dataY(f2,iChan)])
     minX = MIN([dataX(f2,iChan),dataY(f2,iChan)])
     minY = MIN([dataX(f2,iChan),dataY(f2,iChan)])
     posX = minX + xPosRatio * ( maxX - minX )
     posY = minY + (1 - yPosRatio) * ( maxY - minY )
     PLOT, dataX(f2,iChan), dataY(f2,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[1], COLOR = 20
     generateStatsTBvsTB, dataX(f2,iChan), dataY(f2,iChan), statArr_2
     plotStats, statArr_2, posX, posY, (maxY - minY)
   endif else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
   endelse
   if (nf3 ge 2) then begin
     minX = MIN(dataX(f3,iChan))
     maxX = MAX(dataX(f3,iChan))
     minY = MIN(dataY(f3,iChan))
     maxY = MAX(dataY(f3,iChan))
     maxX = MAX([dataX(f3,iChan),dataY(f3,iChan)])
     maxY = MAX([dataX(f3,iChan),dataY(f3,iChan)])
     minX = MIN([dataX(f3,iChan),dataY(f3,iChan)])
     minY = MIN([dataX(f3,iChan),dataY(f3,iChan)])
     posX = minX + xPosRatio * ( maxX - minX )
     posY = minY + (1 - yPosRatio) * ( maxY - minY )
     PLOT, dataX(f3,iChan), dataY(f3,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[2], COLOR = 20
     generateStatsTBvsTB, dataX(f3,iChan), dataY(f3,iChan), statArr_3
     plotStats, statArr_3, posX, posY, (maxY - minY)

   endif else begin
     ; else begin plot a blank plot 
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
   endelse
   if (nf4 ge 2) then begin
     minX = MIN(dataX(f4,iChan))
     maxX = MAX(dataX(f4,iChan))
     minY = MIN(dataY(f4,iChan))
     maxY = MAX(dataY(f4,iChan))
     maxX = MAX([dataX(f4,iChan),dataY(f4,iChan)])
     maxY = MAX([dataX(f4,iChan),dataY(f4,iChan)])
     minX = MIN([dataX(f4,iChan),dataY(f4,iChan)])
     minY = MIN([dataX(f4,iChan),dataY(f4,iChan)])
     posX = minX + xPosRatio * ( maxX - minX )
     posY = maxY - yPosRatio * ( maxY - minY )
     PLOT, dataX(f4,iChan), dataY(f4,iChan), PSYM=3, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, $ 
           MIN_VALUE = minY, MAX_VALUE = maxY, XRANGE=[minX, maxX], YRANGE=[minY, maxY]
     XYOUTS, posX, posY, sfcTypeNames[3], COLOR = 20
     generateStatsTBvsTB, dataX(f4,iChan), dataY(f4,iChan), statArr_4
     plotStats, statArr_4, posX, posY, (maxY - minY)
   endif else begin
     PLOT, [0, 1], [0, 1], PSYM=1, TITLE=titleName, $
           XTITLE = titleX,  YTITLE = titleY, /NODATA
   endelse
 
   DEVICE, /CLOSE
   ; Convert to JPEG file and remove PS file
   ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait = 1
   ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   dentb:
   if (nf1 ge 2) then begin
     x = dataX(f1,iChan)
     y = dataY(f1,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_ocean' 
     sfcType = sfcTypeNames[0]
     drawTb_Density, x, y,  numValid, titleName, titleX,  titleY, fName, statArr_1, sfcType
   endif
   if (nf2 ge 2) then begin
     x = dataX(f2,iChan)
     y = dataY(f2,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_seaice' 
     sfcType = sfcTypeNames[1]
     drawTb_Density, x, y,  numValid, titleName, titleX,  titleY, fName, statArr_2, sfcType
   endif
   if (nf3 ge 2) then begin
     x = dataX(f3,iChan)
     y = dataY(f3,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_land' 
     sfcType = sfcTypeNames[2]
     drawTb_Density, x, y,  numValid, titleName, titleX,  titleY, fName, statArr_3, sfcType
   endif
   if (nf4 ge 2) then begin
     x = dataX(f4,iChan)
     y = dataY(f4,iChan)
     numValid = N_ELEMENTS(x)
     len = STRLEN(imageName)
     fName = STRMID(imageName, 0, len) + '_snow'
     sfcType = sfcTypeNames[3]
     drawTb_Density, x, y,  numValid, titleName, titleX,  titleY, fName, statArr_4, sfcType
   endif

END

PRO generateStats, dataX, dataY, statArr
   ; dataX : goephysical param: tb, skinT, etc
   ; dataY : diff of tb or something else
   ; Define stats var
   statArr=['Correlation: ', 'Mean Bias: ', 'Std Dev: ',  'Slope: ',  $ 
       'Intcpt: ', 'RMS: ', 'NumOfPnts: ', 'Max: ', 'Min: '  ]

   nstat = N_ELEMENTS(statArr)
   stats = fltarr(nstat)
   maxY  = MAX(dataY)
   minY  = MIN(dataY)
   ; Calculate  slope and intercept
   result = POLY_FIT(dataX, dataY, 1, /DOUBLE,STATUS = pfit_stat)

   stats(0) = CORRELATE(dataX, dataY) ; Correlation
   stats(1) = (MOMENT(dataY))[0]      ; Mean Bias
   stats(2) = STDEV(dataY)            ; Std Dev

   IF(pfit_stat EQ 0) THEN BEGIN
      stats(3) = result(1)  ; slope
      stats(4) = result(0)  ; intercept
   ENDIF

   stats(5) = SQRT(MEAN((dataY)^2)) ; RMS
   stats(6) = N_ELEMENTS(dataX)     ; Points
   stats(7) = maxY
   stats(8) = minY
   ;print, 'stats(6)    '  ,   stats(6)

   ; Generate tedataXt to be plotted
   FOR i =0, nstat-1 DO BEGIN
     IF ( i EQ 6 ) THEN $
         statArr[i] = statArr[i] + STRING(LONG(stats(i)))   $
     ELSE               $ 
         statArr[i] = statArr[i] + STRING(stats(i))
     
     statArr[i] = STRCOMPRESS(statArr[i])
   ENDFOR

END

PRO generateStatsTBvsTB, dataX, dataY, statArr
   ; dataX : sim tb
   ; dataY : obs tb
   ; Define stats var
   statArr=['Correlation: ', 'Mean Bias: ', 'Std Dev: ',  'Slope: ',  $ 
       'Intercept: ', 'RMS: ', 'Points: ', 'Max: ', 'Min: '  ]

   nstat = N_ELEMENTS(statArr)
   stats=fltarr(nstat)
   ; Calculate  slope and intercept
   result = POLY_FIT(dataX, dataY, 1, /DOUBLE,STATUS = pfit_stat)

   stats(0) = CORRELATE(dataX, dataY)     ; Correlation
   stats(1) = (MOMENT(dataY - dataX))[0]  ; Mean Bias
   stats(2) = STDEV(dataY - dataX)        ; Std Dev of Bias

   IF(pfit_stat EQ 0) THEN BEGIN
      stats(3) = result(1)  ; slope
      stats(4) = result(0)  ; intercept
   ENDIF

   stats(5) = SQRT(MEAN((dataY - dataX)^2)) ; RMS
   stats(6) = N_ELEMENTS(dataX)       ; Points
   stats(7) = Max(dataY)       ; Max
   stats(8) = Min(dataY)       ; Min

   ; Generate tedataXt to be plotted
   FOR i =0, nstat-1 DO BEGIN
     IF ( i EQ 6 ) THEN $
         statArr[i] = statArr[i] + STRING(LONG(stats(i)))   $
     ELSE               $ 
         statArr[i] = statArr[i] + STRING(stats(i))
     
     statArr[i] = STRCOMPRESS(statArr[i])
   ENDFOR
   
END

PRO plotStats, statArr, posX, posY, yLength
   ;yLength = maxY - minY
   ; Interval Ratio (IR)
   ir = 0.04
   XYOUTS, posX, posY - (ir * 1 * yLength), statArr(0), CHARSIZE = 1, COLOR = 20
   XYOUTS, posX, posY - (ir * 2 * yLength), statArr(1), CHARSIZE = 1, COLOR = 20
   XYOUTS, posX, posY - (ir * 3 * yLength), statArr(2), CHARSIZE = 1, COLOR = 20
   XYOUTS, posX, posY - (ir * 4 * yLength), statArr(3), CHARSIZE = 1, COLOR = 20
   XYOUTS, posX, posY - (ir * 5 * yLength), statArr(4), CHARSIZE = 1, COLOR = 20
   XYOUTS, posX, posY - (ir * 6 * yLength), statArr(5), CHARSIZE = 1, COLOR = 20
   XYOUTS, posX, posY - (ir * 7 * yLength), statArr(6), CHARSIZE = 1, COLOR = 20
   IF (N_ELEMENTS(statArr) gt 7) THEN BEGIN
     XYOUTS, posX, posY - (ir * 8 * yLength), statArr(7), CHARSIZE = 1, COLOR=20
     XYOUTS, posX, posY - (ir * 9 * yLength), statArr(8), CHARSIZE = 1, COLOR=20
   ENDIF    
END

PRO drawTb_Density, dataX, dataY, numValid, titleName, $
      titleX,  titleY, imageName, statsInfo, sfcType
   
  binSize=20
  xBinSize = ( max(dataX) - min(dataX) ) / binSize
  yBinSize = ( max(dataY) - min(datay) ) / binSize
  IF (xBinSize eq 0 or yBinSize eq 0) THEN BEGIN
     print, 'drawTb_Density:  ' + imageName 
     print, '      No valid data.  Plot will not be created.'
     RETURN 
  ENDIF

  ; Find shared min and max for both TB_Obs and TB_Sim
  xrange = PERCENTILE_RANK(dataX,[.1,99.9])
  yrange = PERCENTILE_RANK(dataY,[.1,99.9])
  xmin   = xrange[0]
  xmax   = xrange[1]
  xmin   = MIN([xrange,yrange])
  xmax   = MAX([xrange,yrange])
  ymin   = xmin
  ymax   = xmax
  chsz = 1.
  iplotstats = 1
  DensityScatter,xmin,xmax,ymin,ymax,dataX,dataY,titleX,titleY,titleName,chsz,iplotStats,stats,$
               imageName, xbinsize/8, 1.0, 1, font_size=10

END

PRO drawDTb_Density, dataX, dataY, numValid, titleName, $
      titleX,  titleY, imageName, statsInfo, sfcType, tbDiffValue
   ;binsize=10  ; Good size
   binSize  = 25.
   xBinSize = ( max(dataX) - min(dataX) ) / binSize
   ; Find min and max 
   xMin= min(dataX)
   xMax= max(dataX)
   nn = N_ELEMENTS(dataY)

   ; Find shared min and max for both TB_Obs and TB_Sim
   xrange = PERCENTILE_RANK(dataX,[.1,99.9])
   yrange = PERCENTILE_RANK(dataY,[.1,99.9])
   xmin   = xrange[0]
   xmax   = xrange[1]
   ymin   = yrange[0]
   ymax   = yrange[1]
   yBinSize = ( Ymax - Ymin ) / binSize
   sdvy   = SQRT(VARIANCE(datay))
   ;IF (ymax lt sdvy) THEN ymax = sdvy*1.05
   IF (ymax le sdvy) THEN ymax = sdvy*5.0 else ymax   = yrange[1]
   yBinSize = ( ymax - ymin ) / binSize

   ; Use configured max DTb value 
   ;; yMin= -1 * tbDiffValue
   ;; yMax= tbDiffValue
   
   IF (xBinSize eq 0 or yBinSize eq 0) THEN BEGIN
     print, 'drawDTb_Density:  ' + imageName 
     print, '      No valid data.  Plot will not be created.'
     RETURN 
   ENDIF
  
   chsz = 1.
   iplotstats = 1
;   DensityPlotContour,minX,maxX,minY,maxY,X,Y,xtit,ytit,tit,chsz,iplotStats,stats,$
;                      img_name,xbinsize,ybinsize,symbol_size,ps_out, stats=statsx, statpos=statposx, $
;                      custom_pos=custom_posx, order=orderx, font_size=font_sizex
   DensityPlot,xmin,xmax,ymin,ymax,dataX,dataY,titleX,titleY,titleName,chsz,iplotStats,stats,$
               imageName,xbinsize/6,ybinsize/6,1.2,1, stats=1, statpos=1, $
               custom_pos=custom_posx, order=orderx, font_size=10

   ;print, ' done with TB Diff density !!!!!!!!!!!'
END

PRO plotBiasStratified, data, filterObj, confObj
   ; Get channel infomation
   prefix = confObj.prefix
   chanNo = confObj.chanNo
   chanInfo = confObj.chanInfo
   chanNo_Str = confObj.chanNo_Str
   date = confObj.date
   sensorName = confObj.sensorName

   ;---------------------------------------------
   ; Bias plot vs Lat stratified by surface type
   ;---------------------------------------------
   IF ( (filterObj.f1)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_vs_lat_ch' $
         + chanNo_Str + '_ocean.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Ocean, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsLat, data, chanNo, filterObj.f1, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f2)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_vs_lat_ch'  $
         + chanNo_Str + '_seaice.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Sea ice, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsLat, data, chanNo, filterObj.f2, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f3)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_vs_lat_ch'  $
         + chanNo_Str + '_land.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Land, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsLat, data, chanNo, filterObj.f3, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f4)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_vs_lat_ch' $ 
         + chanNo_Str + '_snow.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Snow, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsLat, data, chanNo, filterObj.f4, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   ;-------------------------------------------------------
   ; Bias plot vs scan position stratified by surface type
   ;-------------------------------------------------------
   IF ( (filterObj.f1)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch'  $ 
         + chanNo_Str + '_ocean.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Ocean, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f1, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f2)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $
         + chanNo_Str + '_seaice.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Sea ice, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f2, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f3)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_land.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Land, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f3, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f4)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_snow.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Snow, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f4, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   ;-------------------------------------------------------------------------
   ; Bias plot vs scan position stratified by surface type in Ascending node
   ;-------------------------------------------------------------------------
   IF ( (filterObj.f1a)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_ocean_asc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Ocean Asc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f1a, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f2a)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_seaice_asc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Sea ice Asc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f2a, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f3a)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $
         + chanNo_Str + '_land_asc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Land Asc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f3a, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f4a)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_snow_asc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Snow Asc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f4a, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   ;-------------------------------------------------------------------------
   ; Bias plot vs scan position stratified by surface type in Descending node
   ;-------------------------------------------------------------------------
   IF ( (filterObj.f1d)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_ocean_desc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Ocean Desc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f1d, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f2d)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $
         + chanNo_Str + '_seaice_desc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Sea ice Desc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f2d, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f3d)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $
         + chanNo_Str + '_land_desc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Land Desc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f3d, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

   IF ( (filterObj.f4d)[0] NE -1 ) THEN BEGIN
      imageName = STRCOMPRESS(prefix + 'bias_ch' $ 
         + chanNo_Str + '_snow_desc.eps',/remove_all)
      titleName = STRCOMPRESS(sensorName + ' Bias (O-B) (Snow Desc, chan ' + chanNo_Str $
         + ': ' + chanInfo + ") " + date )
      plotBiasVsScanPos, data, chanNo, filterObj.f4d, titleName, imageName
      ; Convert to JPEG file and remove PS file
      ;; cgPS2Raster, imageName, /JPEG, Width=900, portrait=1
      ;; File_Delete, imageName, /ALLOW_NONEXISTENT  ; remove .ps file
   ENDIF 

END
