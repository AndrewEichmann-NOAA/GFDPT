;---------------------------------------------------------------------------------
; Summary of all subroutines Taylor Plots 
;
; E.S. Maddy Riverside Tech. Inc. @ NOAA/NESDIS 2014 
;
;---------------------------------------------------------------------------------

;===============================================================
; Name:		taylor_def
;
;
; Type:		IDL Subroutine
;
;
; Description:  Define taylor plot structure
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- taylor            O               taylor plot structure
;       - ndat              I               number of data types
; Subroutines needed:
;       - None
;
; History:  Eric S. Maddy Created 2006
;
;===============================================================
PRO TAYLOR_DEF, taylor, ndat

; create an anonymous structure to hold the data
maxtick = 30

tayplot = {title: '            ', xtitle: $
           STRARR(1), ytitle: STRARR(1), colortable: -1L, $
           colors: LONARR(ndat), crange: [0.,1.], $
           cticks_maj: FLTARR(maxtick), $
           cticks_min: FLTARR(maxtick), nctick_maj: 1L, nctick_min: 1L, $
           anglelims: [0.,90., 90.], skillscore: 0L} 

taylor = {ndat: ndat, refstd: FLTARR(ndat), retstd: FLTARR(ndat), $
          rmserr: FLTARR(ndat), correl: FLTARR(ndat), $
          biaserr: FLTARR(ndat), names: STRARR(ndat), correl_theory_max: 1.0, $
          tayplot: tayplot} 

END 

;===============================================================
; Name:		taylor_plot
;
;
; Type:		IDL Subroutine
;
;
; Description:  Create Taylor plot from input Taylor structure
;
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- taylor            I               taylor plot structure
; Subroutines needed:
;       - None
;
; History:  Eric S. Maddy Created 2006
;
;===============================================================
PRO TAYLOR_PLOT, taylor, legend=legendx, taylordiff=taylordiffx, $
                 epsscl=epssclx, noerase=noerasex, saveplot=saveplotx

; save the current plot position 
possave = !P.POSITION  

;IF (NOT KEYWORD_SET(noerasex)) THEN BEGIN

scaleeps = 0L
IF (KEYWORD_SET(epssclx)) THEN scaleeps=1L 

IF (KEYWORD_SET(legendx)) THEN !P.MULTI=[0,2,1]

; Make a vector of 16 points, A[i] = 2pi/16:  
A = FINDGEN(17) * (!PI*2/16.)  
; Define the symbol to be a unit circle with 16 points,   
; and set the filled flag:  
USERSYM, COS(A), SIN(A), /FILL

skillflg=0L
skillscore = taylor.tayplot.skillscore
IF (skillscore gt 0) THEN skillflg=1L

taylordfflg=0L
IF (KEYWORD_SET(taylordiffx)) THEN taylordfflg=1L
tagnames = TAG_NAMES(taylor) 

; normalize (standardize) the standard errors 
taylor.retstd = taylor.retstd/taylor.refstd 
taylor.refstd = taylor.refstd/taylor.refstd 

; set max for the plots 
mxr = MAX([taylor.refstd, taylor.retstd])*1.5
mxr = 1.5
rad = ROUND([mxr, mxr, mxr])
rad = [1.5, 1.5, 1.5]
angle = taylor.tayplot.anglelims*!DtoR 


CDBCOL
!P.POSITION = [0.1, 0.1, 0.9, 0.9]
IF (scaleeps eq 1) THEN !P.POSITION = [0.12, 0.12, 0.88, 0.88]

; set up the plot 
PLOT, rad, angle, /NODATA, XSTY=5, YSTY=5, /POLAR, COLOR=1, TITLE=taylor.tayplot.title

psav = !P & xsav = !X & ysav = !Y
rad = 1.*(FLTARR(100) + 1.0)
angle = REVERSE(0. + MAX(taylor.tayplot.anglelims)*LINDGEN(100)/FLOAT(100-1))*!DtoR

OPLOT, rad, angle, COLOR=1, LINESTYLE=1, THICK=1, /POLAR

rad = 0.5*(FLTARR(100) + 1.0)
OPLOT, rad, angle, COLOR=1, LINESTYLE=1, THICK=1, /POLAR

rad = 0.25*(FLTARR(100) + 1.0)
OPLOT, rad, angle, COLOR=1, LINESTYLE=1, THICK=1, /POLAR
;; 
rad = mxr*(FLTARR(100) + 1.0)
angle = REVERSE(0. + MAX(taylor.tayplot.anglelims)*LINDGEN(100)/FLOAT(100-1))*!DtoR
OPLOT, rad, angle, /POLAR, COLOR=1


xtitle = taylor.tayplot.xtitle 
ytitle = taylor.tayplot.ytitle 

csizt = 1.25

; Draw the x and y axes through (0, 0):  
AXIS, 0, 0, XAX=0, XTITLE=xtitle, COLOR=1, $
       CHARSIZ=csizt

nmaj = taylor.tayplot.nctick_maj
nmin = taylor.tayplot.nctick_min
rplot1 = taylor.tayplot.cticks_maj(0:nmaj-1)
rplot2 = taylor.tayplot.cticks_min(0:nmin-1)
IF (taylordfflg eq 1) THEN BEGIN
; rplot1 = [-1.*REVERSE(taylor.tayplot.cticks_maj(1:nmaj-1)), $
;           taylor.tayplot.cticks_maj(0:nmaj-1)]
; rplot2 = [-1.*REVERSE(taylor.tayplot.cticks_min(1:nmin-1)), $
;           taylor.tayplot.cticks_min(0:nmin-1)]
; XYOUTS, -2.25, 1.0, ytitle, ORIENTATION=90., CHARSIZ=csizt, $
;         ALIGN=0.5
; AXIS, 0, 0, YAX=0., COLOR=1, $
;       CHARSIZ=csizt
  AXIS, 0, 0, YAX=0, YTITLE=ytitle, COLOR=1, $
      CHARSIZ=csizt

ENDIF ELSE AXIS, 0, 0, YAX=0, YTITLE=ytitle, COLOR=1, $
      CHARSIZ=csizt

angpl1 = ACOS(rplot1)
angpl2 = ACOS(rplot2)

npl1   = N_ELEMENTS(angpl1)
npl2   = N_ELEMENTS(angpl2)
dr1     = 0.05
dr2     = 0.02
dr3     = 0.015
dang    = -0.5*!DtoR
; plot the major tick markers and labels 
FOR ipl = 0L, npl1-1L DO BEGIN
  OPLOT, [rad(0)-dr1,rad(0)], [angpl1(ipl),angpl1(ipl)], $
         COLOR=1, /POLAR

  fmt = "(f7.1)"
  rpl = rplot1(ipl)
  drpl = 0.4*(rpl*10.-FLOOR(rpl*10))
  IF (drpl ne 0) THEN fmt="(f7.2)"
  str = STRTRIM(STRING(rplot1(ipl),fmt),2)
  IF (rpl ge 0) THEN BEGIN
    ango = angpl1(ipl)*180./!pi 
    xpl = (mxr+dr3)*COS(ango*!DtoR + dang)
    ypl = (mxr+dr3)*SIN(ango*!DtoR + dang)
    alignx = -0.105
  ENDIF ELSE BEGIN
    ango = -1.0*(180.-angpl1(ipl)*180./!pi)
    xpl = (mxr+2.*dr3)*COS(angpl1(ipl)-dang)
    ypl = (mxr+2.*dr3)*SIN(angpl1(ipl)-dang)
    alignx = 0.925
  ENDELSE
  XYOUTS, xpl, ypl, str, ALIGN=alignx, $
          ORIENTATION=ango ; , ALIGN=0.5
;  IF (skillflg eq 0) THEN $
  OPLOT, [0.,rad(0)-dr1], [angpl1(ipl),angpl1(ipl)], $
         COLOR=1, /POLAR, LINESTYLE=2, THICK=1

ENDFOR 


; plot the minor tick markers 
FOR ipl = 0L, npl2-1L DO $
  OPLOT, [rad(0)-dr2,rad(0)], [angpl2(ipl),angpl2(ipl)], $
         COLOR=1, /POLAR

IF (skillflg eq 0) THEN BEGIN

std1   = taylor.refstd
std2   = taylor.retstd
cor    = taylor.correl
E2     = std1^2. + std2^2. - 2.0*std1*std2*cor

npts   = 5.
stdcon = 0.0 + (2.0*ROUND(MAX(E2)) - 0.0)*FINDGEN(npts)/FLOAT(npts-1)
nstd   = N_ELEMENTS(stdcon) 

CDBCOL 
dx = (1.-0.15)/FLOAT(npts)
FOR i = 1L, nstd-1L DO BEGIN 
  x      = -1.0*stdcon(i) + 1. + 2.*stdcon(i)*(FINDGEN(1000)/999.)
  y      = SQRT(stdcon(i)*stdcon(i) - (x-1.0)*(x-1.))
  rad    = SQRT((x)*(x) + y*y) 
  idx    = WHERE(rad ge mxr,nidx)
  IF (nidx ne 0) THEN x(idx) = !VALUES.F_NAN
  IF (nidx ne 0) THEN y(idx) = !VALUES.F_NAN
  OPLOT, x, y, COLOR=1, LINESTYLE=3, THICK=1
  
  y  = stdcon(i)
  str = STRTRIM(STRING(stdcon(i),"(f5.1)"),2)
  xo = 1.0 - dx*i
  yo  = SQRT(stdcon(i)*stdcon(i) - (xo - 1.)^2.)
  
  XYOUTS, xo, yo, str
ENDFOR 
  
ENDIF 

IF (skillflg eq 1) THEN BEGIN
  nskill = 200.  
  pow=1.
  IF (skillscore ne 1) THEN pow=4.
  CASE 1 OF 
    skillscore gt 0: BEGIN
;      R0  = MAX(taylor.correl)  
;      r0  = 0.649
;      r0  = 0.7893
      r0   = taylor.correl_theory_max
      r     = 0.0 + 1.5*FINDGEN(nskill+1)/FLOAT(nskill)
      theta = 0. + 180.*FINDGEN(nskill+1)/FLOAT(nskill)
      theta = theta * !DtoR
      S   = FLTARR(nskill+1,nskill+1) + !VALUES.F_NAN
      x   = FLTARR(nskill+1,nskill+1)
      y   = FLTARR(nskill+1,nskill+1)
      FOR i = 0L, nskill DO BEGIN
        r1 = r(i)  
        FOR j = 0L, nskill DO BEGIN
          t1 = theta(j)  
          rect_coord = CV_COORD(FROM_POLAR=[t1,r1], /TO_RECT)  
          x1 = rect_coord(0)
          y1 = rect_coord(1)
          rad = SQRT(x1^2. + y1^2.)
          rc  = COS(t1)
          IF (rad ge 2.) THEN continue
          n1s   = 4.0*(1.0+rc)^pow
          d1s   = (1.0+R0)^pow
          IF (x1 eq 0) THEN continue
          sig1  = r1
          x2   = (sig1 + 1./sig1)^2.
          S(j,i) = n1s/(x2*d1s)
          x(j,i) = x1
          y(j,i) = y1
        ENDFOR
    ENDFOR
    END
ENDCASE
 ulev = 0. + 1.*FINDGEN(11)/10.
 CONTOUR, S, x, y, /FOLLOW, LEVELS=ulev, COLOR=1, /OVERPLOT, $
          C_THICK=1.
ENDIF

CASE 1 OF 
  taylordfflg eq 1L: BEGIN
    IF (MAX(taylor.tayplot.anglelims) le 90.) THEN BEGIN
      corientation = -38.         ; deg 
      x0 = 0.95
      y0 = 1.3
      IF (scaleeps eq 1) THEN BEGIN
        corientation = -36.
        x0 = 0.85
        y0 = 1.42
      ENDIF
      XYOUTS, x0, y0, 'Correlation', ORIENTATION=corientation , $
            CHARSIZ=csizt
    ENDIF ELSE BEGIN
      corientation = 0         ; deg 
      XYOUTS, -0.25, 2.15, 'Correlation', ORIENTATION=corientation , $
            CHARSIZ=csizt
    ENDELSE
  END 
  ELSE: BEGIN
      IF (MAX(taylor.tayplot.anglelims) le 90.) THEN BEGIN
      corientation = -38.         ; deg 
      x0 = 0.95
      y0 = 1.3
      IF (scaleeps eq 1) THEN BEGIN
        x0 = 0.85
        y0 = 1.4
      ENDIF
      XYOUTS, x0, y0, 'Correlation', ORIENTATION=corientation , $
            CHARSIZ=csizt
    ENDIF ELSE BEGIN
      corientation = 0         ; deg 
      x0 = -0.25
      y0 = 2.15
      XYOUTS, x0, y0, 'Correlation', ORIENTATION=corientation , $
            CHARSIZ=csizt
    ENDELSE
  END
ENDCASE   ; taylor plot setup

ndat = taylor.ndat
colortable = taylor.tayplot.colortable 
dialx = 0L
ctbl = colortable
IF (colortable ge -1) THEN BEGIN
  IF (colortable eq -1) THEN dialx=1L 
  crange=taylor.tayplot.crange
  MYCT, ctbl, BOTTOM=3, NCOLORS=ndat, DIAL=DIALX, RANGE=crange
ENDIF  
IF (colortable eq -2) THEN MYCT_SETCOLOR, NCOLORS=ndat, BOTTOM=3, DIFF=1
IF (colortable eq -3) THEN MYCT_SETCOLOR, NCOLORS=ndat, BOTTOM=3, /GRAYSCALE

colors = LINDGEN(ndat)+3

CASE 1 OF 
  taylordfflg eq 1L: BEGIN
    ; normalize (standardize) the standard errors 
    taylor2 = taylordiffx
    taylor2.retstd = taylor2.retstd/taylor2.refstd 
    taylor2.refstd = taylor2.refstd/taylor2.refstd 

    FOR ist = 0L, ndat-1L DO BEGIN
      tmp1 = taylor.refstd(ist)
      tmp2 = taylor.retstd(ist)

      tmp3 = taylor2.refstd(ist)
      tmp4 = taylor2.retstd(ist)
  
      ang1 = ACOS(taylor.correl(ist))
      ang2 = ACOS(taylor2.correl(ist))
      
      x3 = tmp2*COS(ang1)
      x4 = tmp4*COS(ang2)

      y3 = tmp2*SIN(ang1)
      y4 = tmp4*SIN(ang2)
      
      ARROW, x4, y4, x3, y3, COLOR=colors(ist), /DATA, THICK=2, /SOLID
      
    ENDFOR 
  END 
  taylordfflg eq 0L: BEGIN
    FOR ist = 0L, ndat-1L DO BEGIN
      tmp1 = taylor.refstd(ist)
      tmp2 = taylor.retstd(ist)
  
      ang1 = ACOS(taylor.correl(ist))
      
      OPLOT, [tmp2], [ang1], /POLAR, PSYM=8, COLOR=colors(ist)
      OPLOT, [tmp1], [0.], /POLAR, PSYM=8, COLOR=colors(ist)
    ENDFOR 
  END 
ENDCASE  
  

IF (KEYWORD_SET(legendx) AND NOT KEYWORD_SET(noerasex)) THEN BEGIN 
  posleg = [0.74, 0.65, 0.9, 0.95]
  IF (scaleeps eq 1) THEN posleg = [0.71, 0.63, 0.9, 0.985]
  maxstr = MAX(STRLEN(taylor.names))
  IF (maxstr gt 30) THEN posleg = [0.70, 0.65, 0.9, 0.95]

  !P.POSITION = posleg
  xrax = [-0.05, 1.05]
  yrax = [-0.05, 1.05]
  PLOT, xrax, yrax, /NODATA, XRANGE=xrax, YRANGE=yrax, $
        XSTYLE=5, YSTYLE=5
  
  yo = 1. + (0. - 1.) * FINDGEN(ndat) / FLOAT(ndat-1) 
  xo = 0.01
  dx = 0.1
  dy = -0.02
  
  FOR i = 0L, ndat-1L DO BEGIN
    XYOUTS, xo+dx, yo(i)+dy, taylor.names(i), CHARSIZ=0.75
    OPLOT, [xo], [yo(i)], COLOR=colors(i), PSYM=8
  ENDFOR
ENDIF


IF (KEYWORD_SET(saveplotx)) THEN BEGIN
  !P = psav & !X = xsav & !Y = ysav
  saveplotx = possave
  RETURN
ENDIF  
IF (NOT KEYWORD_SET(noerasex)) THEN BEGIN
  !P.MULTI=[0,1,1]
  ; return plot to current saved position 
  !P.POSITION = possave
ENDIF

END 

;npts = 100L
;rad = FLTARR(npts*3)
;theta = FLTARR(npts*3)
;id1 = LINDGEN(npts) 
;rad(id1) = 0.5*(FLTARR(npts)+1.)
;theta(id1) = REVERSE(0. + MAX(taylor.tayplot.anglelims)*LINDGEN(npts)/FLOAT(npts-1L))*!DtoR
;id2 = id1 + npts
;rad(id2) = 0.75*(FLTARR(npts)+1.)
;theta(id2) = REVERSE(0. + MAX(taylor.tayplot.anglelims)*LINDGEN(npts)/FLOAT(npts-1L))*!DtoR
;id2 = id2 + npts
;rad(id2) = 1.0*(FLTARR(npts)+1.)
;theta(id2) = REVERSE(0. + MAX(taylor.tayplot.anglelims)*LINDGEN(npts)/FLOAT(npts-1L))*!DtoR

;;x = FLTARR(npts*2)
;;y = FLTARR(npts*2)
;;FOR i1 = 0L, npts*2-1L DO BEGIN
;;  coords = CV_COORD(FROM_POLAR=[theta(i1),rad(i1)], /TO_RECT)  
;;  x(i1) = coords[0]
;;  y(i1) = coords[1]
;;ENDFOR 

;z = rad

;z = FLTARR(npts*3,npts*3)
;x = FLTARR(npts*3,npts*3)
;y = FLTARR(npts*3,npts*3)
;icnt = 0L
;FOR i1 = 0L, npts*3-1 DO BEGIN
;   FOR i2 = 0L, npts*3-1 DO BEGIN
;      z(i2,i1) = rad(icnt)
;      coords = CV_COORD(FROM_POLAR=[theta(icnt),rad(icnt)], /TO_RECT)  
;      x(i2,i1) = coords[0]
;      y(i2,i1) = coords[1]
;  ENDFOR
;  icnt = icnt + 1
;ENDFOR

;CONTOUR, z, x, y, LEVELS=[0.0, 0.5, 1.], C_THICK=1., C_LINESTYLE=1, /OVERPLOT, COLOR=1, /FOLLOW

;stop
