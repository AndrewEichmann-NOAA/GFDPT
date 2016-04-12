; docformat = 'rst'
;+
; This is an example program to demontrate how to create a density scatter plot
; with Coyote Graphics routines.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Save the program as "density_plot.pro" and run it like this::
;       IDL> .RUN density_plot
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 23 January 2013 by David W. Fanning.
;        April 7, 2014 Input data passed into routine Eric S. Maddy
;        April 17,2014 Plot statistics Deyong Xu
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO density_Plot, x, y, xbinsize, ybinsize, xrange, yrange, $
                  xtitle, ytitle, title, nlevels=nlevels, $
                  stats=statsx, statsInfo, sfcType
 
   ; Set up variables for the plot. Normally, these values would be 
   ; passed into the program as positional and keyword parameters.
   ;  xrange = [Min(x), Max(x)]
   ;  yrange = [Min(y), Max(y)]
   ;  xbinsize = 0.25
   ;  ybinsize = 3.00
;-----------------------------------------------------
;   if (yrange[0] * yrange[1] LT 0 ) then begin
;      if ( abs(yrange[0]) GT abs(yrange[1]) ) then begin 
;          yrange[1] = -1 * yrange[0] 
;      endif  
;      if ( abs(yrange[0]) LT abs(yrange[1]) ) then begin 
;          yrange[0] = -1 * yrange[1] 
;      endif  
;   endif
;-----------------------------------------------------
   
   ; Open a display window.
   cgDisplay
   
   ; Create the density plot by binning the data into a 2D histogram.
   density = Hist_2D(x, y, Min1=xrange[0], Max1=xrange[1], Bin1=xbinsize, $
                           Min2=yrange[0], Max2=yrange[1], Bin2=ybinsize)   
                           
   maxDensity = Ceil(Max(density)/1e2) * 1e2
   scaledDensity = BytScl(density, Min=0, Max=maxDensity)
   ;help, density
   ;print, density
   ; Load the color table for the display. All zero values will be gray.
   cgLoadCT, 33
   TVLCT, cgColor('gray', /Triple), 0
   TVLCT, r, g, b, /Get
   palette = [ [r], [g], [b] ]
   
   ; Display the density plot.
   i1 = SIZE(density,/DIMENSION)
   ;help, i1
   ;print, 'i1 is ', i1
   xd = xrange[0] + xbinsize*FINDGEN(i1[0])
   yd = yrange[0] + ybinsize*FINDGEN(i1[1])
   ;print, 'xd, yd,  ', xd, yd
   cgContour, density, xd, yd, XRange=xrange, YRange=yrange, $
     /Axes, Palette=palette, $
     XTitle=xtitle, YTitle=ytitle, $
     Position=[0.125, 0.125, 0.9, 0.8], /fill, nlevels=nlevels

   OPLOT, [xrange], [0., 0.], THICK=1, LINESTYLE=1, COLOR=cgColor('gray')
   thick = (!D.Name EQ 'PS') ? 4 : 2
   IF (KEYWORD_SET(statsx)) THEN BEGIN
     avgplot = FLTARR(i1[0]) + !VALUES.F_NAN 
     sdvplot = FLTARR(i1[0]) + !VALUES.F_NAN 
     nstplot = LONARR(i1[0])
     FOR i = 0, i1[0]-2L DO BEGIN
       idx = WHERE(x ge xd[i] and x lt xd[i+1],nidx)
       nstplot[i] = nidx
       IF (nidx eq 0) THEN continue 
       avgplot[i] = MEAN(y[idx])
       IF (nidx gt 2) THEN sdvplot[i] = SQRT(VARIANCE(y[idx]))
     ENDFOR
     idx = WHERE(x ge x[i1[0]-1],nidx)
     nstplot[i] = nidx
     IF (nidx ne 0) THEN BEGIN
       avgplot[i1[0]-1] = MEAN(y[idx])  
       IF (nidx gt 2) THEN sdvplot[i1[0]-1] = SQRT(VARIANCE(y[idx]))
     ENDIF
     OPLOT, xd, avgplot, COLOR=cgColor('black'), THICK=thick
     OPLOT, xd, sdvplot, COLOR=cgColor('black'), LINESTYLE=2, THICK=thick
     ;print, avgplot, sdvplot
   ENDIF

   ; Load the color table for the display. All zero values will be gray.
   cgLoadCT, 33
   TVLCT, cgColor('gray', /Triple), 0
   TVLCT, r, g, b, /Get
   palette = [ [r], [g], [b] ]
   cgContour, density, xd, yd, xrange=xrange, yrange=yrange, $
     LEVELS=maxDensity*[0.25, 0.5, 0.75], /Overplot, $
       C_Colors=['Tan','Tan', 'Brown'], C_Annotation=['Q1', 'Q2', 'Q3'], $
       C_Thick=thick, C_CharThick=thick

   ; Plot statistics 
   posRatio = 0.1
   posX = xRange[0] + posRatio * ( xRange[1] - xRange[0] )
   posY = yRange[0] + (1 - posRatio) * ( yRange[1] - yRange[0] )
   yLength = yRange[1] - yRange[0]
   ; Interval Ratio (IR)
   ir = 0.04
   ;print,'--------------------------------------------'
   ;print,  posX, posY - (ir * 1 * yLength), statsInfo(0), yLength  * 0.04
   ;print,'--------------------------------------------'
   XYOUTS, posX, posY, sfcType, CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 1 * yLength), statsInfo(0), CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 2 * yLength), statsInfo(1), CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 3 * yLength), statsInfo(2), CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 4 * yLength), statsInfo(3), CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 5 * yLength), statsInfo(4), CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 6 * yLength), statsInfo(5), CHARSIZE = 1, COLOR=200
   XYOUTS, posX, posY - (ir * 7 * yLength), statsInfo(6), CHARSIZE = 1, COLOR=200
   IF (N_ELEMENTS(statsInfo) gt 7) THEN BEGIN
     XYOUTS, posX, posY - (ir * 8 * yLength), statsInfo(7), CHARSIZE = 1, COLOR=200
     XYOUTS, posX, posY - (ir * 9 * yLength), statsInfo(8), CHARSIZE = 1, COLOR=200
   ENDIF    
   ; Display a color bar.
   cgColorbar, Position=[0.125, 0.875, 0.9, 0.925], Title=title, $
       Range=[0, maxDensity], NColors=254, Bottom=1, OOB_Low='gray', $
       TLocation='Top'
END ;*****************************************************************

;  This main program shows how to call the program and produce
;  various types of output.

;   Display the plot in a graphics window.
;   Density_Plot
  
;    Display the plot in a resizeable graphics window.
;   cgWindow, 'Density_Plot', Background='White', $
;      WTitle='Density Plot in a Resizeable Graphics Window'
  
;    Create a PostScript file.
;   cgPS_Open, 'density_plot.ps'
;   Density_Plot
;   cgPS_Close
  
;    Create a PNG file with a width of 600 pixels.
;   cgPS2Raster, 'density_plot.ps', /PNG, Width=600

; END
