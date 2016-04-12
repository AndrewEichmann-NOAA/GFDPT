function setdiff, a, b, result

  ia = UNIQ(a)
  a = a(ia)
  b = b(UNIQ(b))
  
  ; Find matching entries
  ab = [a,b]

  ndx = SORT(ab)
  result  = ab(ndx)
  
  nc = N_ELEMENTS(result)
  idx1 = LINDGEN(nc-1)
  idx2 = LINDGEN(nc-1)+1
  result1 = result(idx1)
  result2 = result(idx2)
  d = WHERE( result1 eq result2 )
  ; these are the matching entries
  ndx([d,d+1]) = N_ELEMENTS(a)+1L
  d1 = (ndx lt N_ELEMENTS(a)) ; logical entries in a that don't match
  d = WHERE(d1 eq 1)
  IF (d(0) ne -1L) THEN BEGIN
    ia = ia (ndx(d))
    result  = a(ndx(d))
    return, result
  ENDIF ELSE return, -1L
end
function intersect, a, b, n, d=d 

  a = a(UNIQ(a))
  b = b(UNIQ(b))

  ; Find matching entries
  ab = [a,b]
  ndx = SORT(ab)
  result  = ab(ndx)
  
  nc = N_ELEMENTS(result)
  idx1 = LINDGEN(nc-1)
  idx2 = LINDGEN(nc-1)+1
  result1 = result(idx1)
  result2 = result(idx2)
  d = WHERE( result1 eq result2 )
  
  IF (d(0) ne -1L) THEN BEGIN 
    ndx = ndx([d,d+1])
    result = result(d)
    n = N_ELEMENTS(a)
    return, result 
  ENDIF ELSE return, -1 
 
end
PRO GSI_BLACKLIST, itype, zenith, presw, tropprs, iuse
  
;+
;; NAME: gsi_blacklist
;; 
;; PURPOSE: Emulate GSI blacklisting of satwind observations
;; 
;; REFERENCES : 
;;   http://research.metoffice.gov.uk/research/interproj/nwpsaf/satwind_report/amvusage/ncepmodel.html
;;   gsi/src/main/setupw.f90
;;   2014-04-24
;;
;; Zenith angle greater than 68 for satellite winds from geostationary satellites
;; Pressure level less than 125 hPa, <------------- (esm: doesn't
;;                                                   seem to be in  14/04/24
;;                                                   setupw.f90
;;                                                   it's in
;;                                                   read_satwnd.f90 14/05/06)  
;;        or 
;;        greater than 950 hPa
;; No visible data used if pressure level less than 700 hPa
;; Pressure level is 50 hPa above tropopause
;; NESDIS IR and EUMETSAT IR between 800-400 hPa
;; JMA IR between 800-500 hPa removed
;; Water vapor cloud top wind height below 400hPa
;; Pressure below 600 hPa for MODIS water vapor cloud top wind
;; Height above 250 hPa for MODIS IR wind
;; IR winds over land
;; 600-250 hPa for MODIS water vapor deep layer wind
;; IR winds over land <---- not tested 
;;
;; AUTHOR: Eric S. Maddy RTi @ JCSDA
;; 
;; HISTORY: 2014/04/24    Created 
;-

  viswind_eumetsatjma_limit = 700.0
  allwind_limit             = [125.0, 950.0]
  irwind_goes_limit         = [399.0, 801.0]
  irwind_jma_limit          = [499.0, 801.0]
  irwind_eum_limit          = [401.0, 801.0]
  irwind_wvctp_limit        = 399.0
  wind_modis_limit          = [249.0, 600.0, 600.0, 249.0]
  geozenith_limit           = 68.0 

  nobs= N_ELEMENTS(itype) 
  ;  iuse = 1 
  ;  think happy for now 
  iuse = LONARR(nobs) + 1
  ;--- screen large zenith angles from geostationary satellites
  ;    241: India, 242:JMA visible,243: EUMETSAT visible
  ;    245: GOES IR. 246: GOES WV cloud top, 247: GOES WV deep layer
  ;    250: JMA WV deep layer. 251:GOES visible, 252: JMA IR
  ;    253: EUMETSAT IR , 254: EUMETSAT WV deep layer
  idx = WHERE(itype eq 241 or itype eq 242 or $
              itype eq 243 or itype eq 245 or $
              itype eq 246 or itype eq 247 or $
              itype eq 250 or itype eq 251 or $
              itype eq 252 or itype eq 253 or $
              itype eq 254,nidx)

  IF (nidx ne 0) THEN BEGIN
     idg1 = WHERE(ABS(zenith[idx]) gt geozenith_limit,nidg1)
     IF (nidg1 gt 0) THEN iuse[idx[idg1]] = 0
  ENDIF

  ;--- screen data below 950mb
  idx = WHERE(itype ge 242 and itype le 256,nidx)
  IF (nidx ne 0) THEN BEGIN
     idg1 = WHERE(presw[idx] gt allwind_limit[1] or presw[idx] lt allwind_limit[0],nidg1)
     IF (nidg1 gt 0) THEN iuse[idx[idg1]] = 0
  ENDIF

  ;--- screen data 50mb above tropopause
  idg1 = WHERE(presw lt tropprs-50.0,nidg1)
  IF (nidg1 gt 0) THEN iuse[idg1] = 0

  ;--- visible winds from JMA and EUMETSAT (p < 700hPa) 
  i1 = WHERE(itype eq 242 or itype eq 243,ni1)
  IF (ni1 ne 0) THEN BEGIN
    ip1 = WHERE(presw[i1] lt viswind_eumetsatjma_limit,nip1)
    IF (nip1 ne 0) THEN iuse[i1[ip1]] = 0
  ENDIF
  ;--- GOES IR winds 
  i1 = WHERE(itype eq 245,ni1)
  IF (ni1 ne 0) THEN BEGIN
    ip1 = WHERE(presw[i1] gt irwind_goes_limit[0] and presw[i1] lt irwind_goes_limit[1],nip1)
    IF (nip1 ne 0) THEN iuse[i1[ip1]] = 0
  ENDIF
  ;--- JMA IR winds
  i1 = WHERE(itype eq 252,ni1)
  IF (ni1 ne 0) THEN BEGIN
    ip1 = WHERE(presw[i1] gt irwind_jma_limit[0] and presw[i1] lt irwind_jma_limit[1],nip1)
    IF (nip1 ne 0) THEN iuse[i1[ip1]] = 0
  ENDIF
  ;--- EUMETSAT IR winds
  i1 = WHERE(itype eq 253,ni1)
  IF (ni1 ne 0) THEN BEGIN
    ip1 = WHERE(presw[i1] gt irwind_eum_limit[0] and presw[i1] lt irwind_eum_limit[1],nip1)
    IF (nip1 ne 0) THEN iuse[i1[ip1]] = 0
  ENDIF
  ;--- WVCTP winds
  i1 = WHERE(itype eq 246 or itype eq 250 or itype eq 254,ni1)
  IF (ni1 ne 0) THEN BEGIN
    ip1 = WHERE(presw[i1] gt irwind_wvctp_limit,nip1)
    IF (nip1 ne 0) THEN iuse[i1[ip1]] = 0
  ENDIF
  ;--- MODIS winds 
  ;--- 257: MODIS IR, 258: WV cloud top, 259:  WV deep layer
  i1 = WHERE(itype eq 257 and presw lt wind_modis_limit[0],ni1)
  IF (ni1 ne 0) THEN iuse[ip1] = 0
  i1 = WHERE(itype eq 258 and presw gt wind_modis_limit[1],ni1)
  IF (ni1 ne 0) THEN iuse[ip1] = 0
  i1 = WHERE(itype eq 259 and presw gt wind_modis_limit[2],ni1)
  IF (ni1 ne 0) THEN iuse[ip1] = 0
  i1 = WHERE(itype eq 259 and presw lt wind_modis_limit[3],ni1)
  IF (ni1 ne 0) THEN iuse[ip1] = 0

  ;--- QC MODIS winds
  i1 = WHERE(itype eq 257 or itype eq 258 or itype eq 259,ni1)

  RETURN 
  IF (ni1 ne 0) THEN BEGIN  ; these tests require forecast/guess fields -- skip them
     
    ; Get guess values of tropopause pressure and sea/land/ice
    ; mask at observation location
    prsfc = prsfc[i1]       ; surface pressure in hPa

    ; Compute observed and guess wind speeds (m/s).  
    spdges = sqrt(ugesin[i1]* ugesin[i1] +vgesin[i1]* vgesin[i1] )
 
    ; Set and computes modis specific qc parameters
    qcu = 7.
    qcv = 7.
    qc_spd = (spdges+15.)/3.0
    qc_prs = 0.0
    IF (itype eq 257) THEN qc_prs = prsfc - 200.0
    IF (itype eq 258 or itype eq 259) THEN qc_prs = 400.0
    IF ( presw gt qc_prs and qc_spd lt qcu ) THEN BEGIN
      qcu = (spdob + r15)/three
      qcv = (qcv*qcu)/r7
    ENDIF

    ;; IF(abs(dudiff) > qcu .or. &                      !  u component check
    ;;         abs(dvdiff) > qcv .or. &                      !  v component check
    ;;         (presw > prsfc-r200 .and. isli /= 0))then ! near surface check
    ;;        error = zero
    ;;     endif
    ;;  endif                                                  ! end if all satellite winds
  ENDIF 
     
END
FUNCTION WINDANGLE, u, v, $
                    WINDSPEED=windspeed

rundef = -9999.

;---check input
nu = n_elements(u) 
nv = n_elements(v) 
IF ( (nu ne nv) or (nu eq 0) ) THEN BEGIN
   print,'Number of elements in u and v are not equal'
   RETURN, rundef
ENDIF

;---calculate wind angle
rad2deg   = 180./!PI
windang   = 90.-ATAN(-v,-u)*rad2deg

;---keep range within 0, 360.
id1 = WHERE(windang lt 0.,nid1)
IF (nid1 ne 0) THEN windang[id1] += 360.0

;---optional return argument
windspeed = SQRT(u*u+v*v)

;---return wind angle
RETURN, windang

END
PRO MERGE_AMV, amv1, amv2, amv_merge

;--- get number of profiles and number of QC variables
nobs1 = N_ELEMENTS(amv1.lat)
nobs2 = N_ELEMENTS(amv2.lat)
nQC1  = N_ELEMENTS(amv1.qc[0,*])
nQC2  = N_ELEMENTS(amv2.qc[0,*])

IF (nQC1 ne nQC2) THEN stop

;--- total number of profiles 
nProfiles = nobs1 + nobs2
;--- create output structure
DEF_SCENEAMV, amv_merge, nQC1, nProfiles

;--- indices of first AMV file obs. in output structure
lidx1 = LINDGEN(nobs1)

;--- indices of second AMV file obs. in output structure
lidx2 = LINDGEN(nobs2) + nobs1

;--- merge input into output structure
amv_merge.ProfIndx[lidx1]        = amv1.ProfIndx
amv_merge.AlgDesc[lidx1]         = amv1.AlgDesc
amv_merge.HeightMethod[lidx1]    = amv1.HeightMethod
amv_merge.WindCalcMethod[lidx1]  = amv1.WindCalcMethod
amv_merge.angle[lidx1]           = amv1.angle
amv_merge.WindSp[lidx1]          = amv1.WindSp
amv_merge.WindDir[lidx1]         = amv1.WindDir
amv_merge.WindU[lidx1]           = amv1.WindU
amv_merge.WindV[lidx1]           = amv1.WindV
amv_merge.WindPress[lidx1]       = amv1.WindPress
amv_merge.WindError[lidx1]       = amv1.WindError
amv_merge.qc[lidx1,*]            = amv1.qc
amv_merge.qi[lidx1,*]            = amv1.qi
amv_merge.lat[lidx1]             = amv1.lat
amv_merge.lon[lidx1]             = amv1.lon
amv_merge.node[lidx1]            = amv1.node
amv_merge.scanUTC[lidx1]         = amv1.scanUTC
amv_merge.scanyear[lidx1]        = amv1.scanyear
amv_merge.scanmonth[lidx1]       = amv1.scanmonth
amv_merge.scanday[lidx1]         = amv1.scanday

amv_merge.ProfIndx[lidx2]        = amv2.ProfIndx
amv_merge.AlgDesc[lidx2]         = amv2.AlgDesc
amv_merge.HeightMethod[lidx2]    = amv2.HeightMethod
amv_merge.WindCalcMethod[lidx2]  = amv2.WindCalcMethod
amv_merge.angle[lidx2]           = amv2.angle
amv_merge.WindSp[lidx2]          = amv2.WindSp
amv_merge.WindDir[lidx2]         = amv2.WindDir
amv_merge.WindU[lidx2]           = amv2.WindU
amv_merge.WindV[lidx2]           = amv2.WindV
amv_merge.WindPress[lidx2]       = amv2.WindPress
amv_merge.WindError[lidx2]       = amv2.WindError
amv_merge.qc[lidx2,*]            = amv2.qc
amv_merge.qi[lidx2,*]            = amv2.qi
amv_merge.lat[lidx2]             = amv2.lat
amv_merge.lon[lidx2]             = amv2.lon
amv_merge.node[lidx2]            = amv2.node
amv_merge.scanUTC[lidx2]         = amv2.scanUTC
amv_merge.scanyear[lidx2]        = amv2.scanyear
amv_merge.scanmonth[lidx2]       = amv2.scanmonth
amv_merge.scanday[lidx2]         = amv2.scanday

END
PRO EMLEGEND, xrange, yrange, labels, colors, linestyles, xlog=xlogx, $
            ylog=ylogx, pos=posx, charsize=charsizex, dpos=dposx, $
            center=centerx, psym=psymx, xpos=xposx, ypos=yposx, $
            backfill=backfillx, textcolor=textcolorx, _extra=_extra


nlbl = N_ELEMENTS(labels)
ncol = N_ELEMENTS(colors)
nsty = N_ELEMENTS(linestyles)
IF (nlbl ne ncol) THEN BEGIN
 PRINT, $
 FORMAT="(10x,'Number of labels must be the same as the number of colors')"
ENDIF 

IF (nlbl ne nsty) THEN BEGIN
 PRINT, $
 FORMAT="(10x,'Number of labels must be the same as the number of linestyles')"
ENDIF 

chsiz = 1.0*(3.0/FLOAT(nlbl))
chsiz = 1.0
IF KEYWORD_SET(charsizex) THEN chsiz=charsizex
xlogplot=0L
IF (KEYWORD_SET(xlogx)) THEN xlogplot=1L

ylogplot=0L
IF (KEYWORD_SET(ylogx)) THEN ylogplot=1L

legpos = 0
IF (KEYWORD_SET(posx)) THEN BEGIN
  legpos=posx
  IF (legpos ne 0 and legpos ne 1 and legpos ne 2 and legpos ne 3) THEN stop
;  IF (legpos eq 0 or legpos eq 2) THEN alignx=.5 ELSE alignx=0.0
ENDIF 

xpos = [-0.325,0.175,-0.325,0.195]
ypos = [0.45, 0.45, 0.8, 0.8]
IF (KEYWORD_SET(centerx)) THEN BEGIN
 xpos = -0.25
 ypos = 0.75
ENDIF 

IF (KEYWORD_SET(xposx)) THEN BEGIN
    xpos=xposx
    legpos = 0L
ENDIF 

IF (KEYWORD_SET(yposx)) THEN BEGIN
    ypos=yposx
    legpos = 0L
ENDIF 

IF (xlogplot eq 0L) THEN BEGIN
  x0 = MEAN(xrange) + xpos(legpos)*(xrange(1)-xrange(0)) 
  dx = 0.05*(xrange(1)-xrange(0))
  xlims = [x0-2.0*dx,x0-0.5*dx]
ENDIF ELSE BEGIN
  x0 = 10.^(ALOG10(xrange(0)) + $
       (xpos(legpos)+0.5)*ALOG10(xrange(1)/xrange(0)) )
  dx = ALOG10(xrange(1)/xrange(0))
  dx1 = 10.0^(ALOG10(x0) - 0.1*dx)
  dx2 = 10.0^(ALOG10(x0) - 0.025*dx)
  xlims = 10.^([ALOG10(x0)-ALOG10(dx1),ALOG10(x0)-ALOG10(dx2)])
  xlims = [dx1,dx2]
;  stop
ENDELSE 

dy = 0.25 
IF (KEYWORD_SET(dposx)) THEN dy = dposx

IF (ylogplot eq 0L) THEN BEGIN
  y0 = yrange(0) + ypos(legpos)*(yrange(1)-yrange(0))
  y1 = yrange(0) + (ypos(legpos)-dy)*(yrange(1)-yrange(0))
  ylbl = y0 - (y0 - y1)*FINDGEN(nlbl)/FLOAT(nlbl-1L)
  dy1  = ABS(y0 - y1)
ENDIF ELSE BEGIN
  y0 = 10.^(ALOG10(yrange(0)) + ypos(legpos)*ALOG10(yrange(1)/yrange(0)) )
  y1 = 10.^(ALOG10(yrange(0)) + $
       (ypos(legpos)-dy)*ALOG10(yrange(1)/yrange(0)) )
  ylbl = ALOG10(y0) - (ALOG10(y0/y1))*FINDGEN(nlbl)/FLOAT(nlbl-1L)
  ylbl = 10.0^ylbl
  dy1  = ABS(ALOG10(y0) - ALOG10(y1))
ENDELSE 
IF (KEYWORD_SET(backfillx)) THEN BEGIN
   x01 = MIN(xlims)
   x1 = MAX(xlims) + MAX(STRLEN(labels))*dx/3.
   xx = [x01, x1, x1, x01, x01]
   yy = [y0+dy, y0+dy, y1-dy, y1-dy, y0+dy]
   POLYFILL, xx, yy, COLOR=0, /FILL
ENDIF
;dy1 = 0.01*dy1
textcolor = 1
IF (KEYWORD_SET(textcolorx)) THEN textcolor=textcolorx
FOR ilbl = 0L, nlbl-1L DO BEGIN 
  lsty = linestyles(ilbl)
;  print, lsty, colors[ilbl], labels[ilbl]
  IF (lsty ge 0 and colors[ilbl] gt 0) THEN BEGIN
    OPLOT, xlims, [ylbl(ilbl),ylbl(ilbl)], $
           color=colors(ilbl), linestyle=lsty
  ENDIF ELSE BEGIN
    IF (nlbl le 40) THEN dy1 = dy*0.45
    y1 = ylbl(ilbl)+dy1
    IF (colors[ilbl] gt 0) THEN $
    OPLOT, [xlims(0), MEAN(xlims), xlims(1)], $
           [y1,y1,y1], $
           color=colors(ilbl), psym=ABS(lsty)
  ENDELSE
  y1 = ylbl(ilbl) ;+ dy1
  xyouts, x0, y1, labels(ilbl), $
          charsiz=chsiz, align=alignx, COLOR=textcolor, _extra=_extra
ENDFOR
END 

PRO ScatterMultiple, nunq, strall, adu, xref, xret, $
                     xtitle, ytitle, title, xrax, yrax, img_name, ps_out, $
                     order=orderx, font_size=font_sizex

A  = FINDGEN(17)*!PI*2./16.
USERSYM, COS(A), SIN(A), /FILL
if (ps_out eq 0) then begin
   set_plot,'Z'
   device,set_resolution=[650, 500]
endif
if (ps_out eq 1) then begin
   close,/all
   erase
   !p.multi=1
   !p.font=0
   set_plot,'ps'
   xz = 18 & yz = 18
   font_size=16
   IF (KEYWORD_SET(font_sizex)) THEN font_size=font_sizex
   device,filename=img_name+'.ps',$
          /color,bits_per_pixel=8,xsize=xz,ysize=yz,$
          xoffset=1,yoffset=1,/encapsulated,$
          /portrait,font_size=font_size,/bold,/courier
endif

colors = LONG(222*LINDGEN(nunq)/FLOAT(nunq-1) + 3)
bottom = 3
LOADCT, 39, NCOLORS=nunq+1, BOTTOM=bottom
TVLCT, rr, gg, bb, /get
rr(0)=255   & gg(0)=255   & bb(0)=255 ; Load White Color - 0 for background 
rr(255)=0   & gg(255)=0   & bb(255)=0 ; Load Black Color - 255

PLOT, xrax, yrax, XRANGE=xrax, YRANGE=yrax, XTITLE=xtitle, YTITLE=ytitle, $
  TITLE=title, COLOR=bottom

colors = LINDGEN(nunq)+bottom
legstr = STRARR(nunq)
FOR iu = 0L, nunq-1L DO BEGIN

   adui = adu[iu]
   idu = WHERE(strall eq adui,ndu)
   IF (ndu eq 0) THEN continue 
   ipos = STRPOS(adui,'winds')
   IF (ipos ne -1) THEN adui = STRMID(adui,0,ipos-1)
   ipos = STRPOS(adui,'cloud top')
   IF (ipos ne -1) THEN adui = STRMID(adui,0,ipos-1) + ' ctp'
   ipos = STRPOS(adui,'deep')
   IF (ipos ne -1) THEN adui = STRMID(adui,0,ipos-1) + ' dlw'
   ipos = STRPOS(adui,'Ch 4')
   IF (ipos ne -1) THEN adui = STRMID(adui,0,ipos-1)  
   xrf = xref[idu]
   xrt = xret[idu]
   sd  = SQRT(VARIANCE(xrf-xrt))
   bs  = MEAN(xrf-xrt)
   str = STRING(STRTRIM(adui,2), ndu, sd, bs, "(a,',n=',i7,',sd=',f5.2,',bi=',f5.2)" )
   legstr[iu] = STRCOMPRESS(STRTRIM(str,2))
   OPLOT, xrf, xrt, PSYM=8, COLOR=colors[iu], SYMSIZE=0.1

ENDFOR 
oplot, xrax, yrax, COLOR=bottom
linestyles = [LONARR(nunq)-8]
EMLEGEND, xrax, xrax, legstr, colors, linestyles, xlog=xlogx, $
  ylog=ylogx, pos=posx, charsize=0.8, dpos=0.5, $
  center=centerx, psym=psymx, xpos=-0.12, ypos=0.95, $
  backfill=0, textcolor=bottom

  ;---- write out image file
  if (ps_out eq 0) then write_png, img_name, TVRD(), rr, gg, bb, order=orderx
  device,/close

END
