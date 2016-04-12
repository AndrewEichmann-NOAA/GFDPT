;---------------------------------------------------------------------------------
; Name:  plotBiasVsLat.pro
;
; Type:  IDL Program
;
; Description:
;   A local procedure used by main-level assessment tool code
;   to plot bias (O-B)
;
; Author: Deyong Xu (RTI) @ JCSDA,
;         Deyong.Xu@noaa.gov
; Version: Apr 24, 2014, DXu, Initial coding
;
;
;---------------------------------------------------------------------------------
PRO plotBiasVsLat, data, chanNo, filter, titleName, imageName
   ; Get filtered data
   latFilt = (data.lat)(filter)
   dTbsFilt = (data.tbDiff(*, chanNo))(filter)

   ; Create 181-element bin
   FLOAT_FILL_Value = -999
   latBin = INTARR(181)
   latArr = FLTARR(180)
   biasArr = MAKE_ARRAY(180, /FLOAT, VALUE = FLOAT_FILL_Value)

   FOR i = -90, 90 DO BEGIN 
      ; Index j: 0, 1,..., 180
      j = i + 90
      latBin(j) = i
   END 

   ; Create an array in the middle of each bin.
   FOR i = -90, 89 DO BEGIN 
      ; Index j: 0, 1,..., 179
      j = i + 90
      latArr(j) = i + 0.5
   END 

   ; Calculate bias in each bin: [-90, -89], [-89, -88], ..., [89, 90] 
   FOR i = -90, 89 DO BEGIN 
      ; Index j: 0, 1,..., 179
      j = i + 90
      counter = 0
      ; Find all points in a bin
      tmpFilt = WHERE (latFilt GE latBin(j)  $
                   AND latFilt LT latBin(j+1), counter)
      IF ( counter GT 0 ) THEN biasArr(j) = MEAN(dTbsFilt(tmpFilt))
   end 

   ; Define plot location 
   x0 = 0.1
   y0 = 0.2
   x1 = 0.90
   y1 = 0.95

   LOADCT, 39
   ERASE
   !P.MULTI = [0,1,1]
   !P.FONT = 2
   aspect_ratio = 0.8 ;rectangle
   xSizeVal = 20
   ySizeVal = xSizeVal * aspect_ratio
   DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=5,  $
	   /PORTRAIT, FONT_SIZE=11, /BOLD, /COURIER

   ; Specify plot seting
   nonFill = where(biasArr ne FLOAT_FILL_Value)
   minBias = min(biasArr(nonFill))
   maxBias = max(biasArr(nonFill))
   maxX    = MAX(ABS([minBias,maxBias]))
   minBias = -1.*maxX
   maxBias = maxX

   ; Flag to show first plot command, so following oplot can share the axes.
   flag=0
   FOR i = -90, 89 DO BEGIN 
      ; Index j: 0, 1,..., 179
      j = i + 90
      ; Only plot non-fill value.
      IF (flag eq 0) THEN BEGIN 
	 IF (biasArr(j) NE FLOAT_FILL_Value) THEN BEGIN 
	    flag = 1
	    PLOT, [biasArr(j)], [latArr(j)], PSYM = -2, $
	       TITLE = titleName, $
	       XTITLE = 'Bias (O-B) (k)', YTITLE='Lat',  $
	       XRANGE = [minBias, maxBias],   $
	       YRANGE = [-90, 90], POSITION = [x0, y0, x1, y1]
	 ENDIF 
      ENDIF 

      ; Once first plot is used, now use oplot
      ; Again, only plot non-fill value.
      IF (flag EQ 1) THEN BEGIN 
	 IF (biasArr(j) NE FLOAT_FILL_Value) THEN BEGIN 
	    OPLOT, [biasArr(j)], [latArr(j)], PSYM = -2
         ENDIF
      ENDIF 
   ENDFOR

   DEVICE, /CLOSE
END

