;---------------------------------------------------------------------------------
; Name:  plotBiasVsScanPos.pro
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
PRO plotBiasVsScanPos, data, chanNo, filter, titleName, imageName
   ; Get filtered data
   scanPosFilt = data.scanPos(filter)
   ;dTbsFilt = data.tbDiff(filter)
   dTbsFilt = (data.tbDiff(*, chanNo))(filter)
  
   ; Get number scan positions
   POS_NUM = max(scanPosFilt) - min(scanPosFilt) + 1
 
   ; Create arrays (1-based) 
   scanPosArr = INDGEN(POS_NUM) + 1 
   dTbArr = FLTARR(POS_NUM)
   numArr = LONARR(POS_NUM)

   ; Compute bias (mean(DTbs)) for each scan position
   FOR i = 1, POS_NUM DO BEGIN 
      fil = WHERE(scanPosFilt EQ i)
      dTbArr(i-1) = MEAN( dTbsFilt(fil) )
      numArr(i-1) = N_ELEMENTS(fil)
      ;print, 'num of points ', numArr(i-1) 
   ENDFOR

   ; Define plot location 
   LOADCT, 39
   ERASE
   !P.MULTI = [0,1,1]
   !P.FONT = 2
   aspect_ratio = 0.8 ;rectangle
   xSizeVal = 20
   ySizeVal = xSizeVal * aspect_ratio
   DEVICE, FILENAME=imageName, /COLOR, BITS_PER_PIXEL=8,          $
	   XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=5,  $
	   ;XSIZE=xSizeVal, YSIZE=ySizeVal, XOFFSET=2, YOFFSET=5,  $
	   ;/PORTRAIT, FONT_SIZE=11, /BOLD, /COURIER
	   /PORTRAIT, FONT_SIZE=7, /BOLD, /COURIER

   ; Specify plot seting
   maxX    = MAX(ABS([min(dTbArr),max(dTbArr)]))
   minBias = -1.*maxX
   maxBias = maxX

   ; Plot the damped sine curve. Suppress drawing the right-hand Y axis.
   cgPlot, scanPosArr, dTbArr, YTITLE = 'Bias (O-B) (k)', XTITLE='Scan Position', $
      YRANGE = [minBias, maxBias], YStyle=8, Color='red7', PSYM=-4

   ; Draw a second, logarithmic axis on the right-hand side of the plot.
   Axis, YAxis=1, YRange=[min(numArr), max(numArr)], /Save
   ;print,' min(numArr), max(numArr)  ',  min(numArr), max(numArr)

   ; Overplot the logarithmic data.
   cgOplot, scanPosArr, numArr, Color='blu7', PSYM=-5

   ; Plot legend
   x0 = !D.x_size * 0.75
   x1 = !D.x_size * 0.78
   y0 = !D.y_size * 0.85
   y1 = !D.y_size * 0.88

   ;print, x0, y0
   PLOTS, [x0,x0], [y1,y1], PSYM=4, SYMSIZE=2, COLOR=0, /DEVICE
   xyouts, x1, y1, 'bias', color=0, charsize=1, /DEVICE

   PLOTS, [x0,x0], [y0,y0], PSYM=5, SYMSIZE=2, COLOR=0, /DEVICE
   xyouts, x1, y0, 'num of ptns', color=0, charsize=1, /DEVICE

   DEVICE, /CLOSE
END

