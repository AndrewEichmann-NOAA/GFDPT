@io_sceneamv.pro
@AMV_Util.pro
@taylor_plot.pro
@src/idl/mirs_idl/stats_sub.pro
@src/idl/mirs_idl/misc.pro
@src/idl/mirs_idl/utilities.pro
PRO AMV_ASSESSMENT, sceneamv, nwpamv, amvOptions

;===============================================================
; Name:		amv_assement
;
; Type:		IDL Subroutine
;
; Description:  assess AMV satellite winds versus NWP stratifying by
; various ancillary info.
;
; Arguments:
;
;	    Name	    Type	    Description
;      ---------------------------------------------------
;	- SceneAMV          I               SceneAMV structure
;	- NWPAMV            I               SceneAMV structure for NWP
;       - amvOptions        I               Structure containing
;                                           options for amv_assessment
;
;
; History:  Created 5/21/2014
;
; E.S. Maddy Riverside Tech. Inc. @ NOAA/NESDIS 2014 
;===============================================================

;--------------------------------------------------------------------
;---setup options for data filtering, plotting, and output data paths
;--------------------------------------------------------------------
;---plot only high level winds
highlevel    = amvOptions.highlevel
;---plot only low level winds
lowlevel     = amvOptions.lowlevel

;---perform blacklisting as in GSI
gsiblacklist = amvOptions.gsiblacklist
;---check data quality mark  
; (see src/bufr/amv_bufr/AMVBUFR_to_AMVScene)
qualityMark  = amvOptions.qualityMark
;---use ECMWF quality settings
ecmwfqi      = amvOptions.ecmwfqi

;---path for figures
figPath      = amvOptions.figPath
;---figure name prefix
figStr       = amvOptions.figStr
;---NWP source string (e.g., GFS, ECMWF, ...)
nwpstr       = amvOptions.nwpStr
;---time of files string ('00', '12', ...)
time         = amvOptions.TimeStr
;---date string (e.g., 2014-04-01)
date         = amvOptions.date
;---order flag for png output 
order        = amvOptions.pngOrderflag

;---plot (ps = 1, or png = 0)
psplot       = amvOptions.psorpng 

instrument   = amvOptions.sensor
istr         = STRTRIM(instrument,2)
fontsize = 13
figSuf       = '.png'
IF (psplot eq 1) THEN figSuf = ''  ; taken care of in src/idl/mirs_idl/misc.pro
IF (psplot eq 1) THEN $
  PRINT, FORMAT="('User requested postscript output.',/,'Warning: Maps will be created as .png')"

;---set vertical pressure boundaries of data considered
plim = [0., 1100.]
IF (highlevel) THEN plim = [100.,  500.]
IF (lowlevel) THEN  plim = [500., 1100.]

idx = WHERE(sceneamv.lon gt 180.,nidx)
IF (nidx ne 0) THEN sceneamv.lon[idx] -= 360.

idx = WHERE(sceneamv.WindDir gt 180.,nidx)
IF (nidx ne 0) THEN sceneamv.WindDir[idx] -= 360.

idx = WHERE(nwpamv.WindDir gt 180.,nidx)
IF (nidx ne 0) THEN nwpamv.WindDir[idx] -= 360.

;---testing code 
winddir = sceneamv.winddir
windu   = sceneamv.windu
windv   = sceneamv.windv
;sceneamv.windu = sceneamv.windsp*COS(-!PI/180. * winddir) 
;sceneamv.windv = sceneamv.windsp*SIN(-!PI/180. * winddir) 

;---for rapidscat :: here's the definition
;sceneamv.windu = -windv
;sceneamv.windv = windu
;stop
;--------------------------------------------
; make values are reported, QC flag = 0 and 
; QI[*,3] = quality mark not questionable 
; (see src/bufr/amv_bufr/AMVBUFR_to_AMVScene)
;--------------------------------------------
IF (qualityMark) THEN BEGIN
  iFilter = WHERE(sceneamv.windv gt -150. and nwpamv.windv gt -150. and $
              sceneamv.qc[*,2] eq 0 and $
              (sceneamv.qi[*,3] ne 15. and sceneamv.qi[*,3] ne 9. and $
               sceneamv.qi[*,3] ne 12.) and $
              sceneamv.windpress ge plim[0] and $
              sceneamv.windpress le plim[1],niFilter, COMPLEMENT=iFilterc)
  
ENDIF ELSE BEGIN
  ; old - no QI => quality mark 
  iFilter = WHERE(sceneamv.windv gt -150. and nwpamv.windv gt -150. and $
              sceneamv.lat gt -90.1 and $
              sceneamv.windpress ge plim[0] and $
              sceneamv.windpress le plim[1],niFilter, COMPLEMENT=iFilterc)
ENDELSE

windsp_lim = 45.
IF (STRTRIM(STRMID(instrument,0,4),2) eq 'AVHR') THEN BEGIN
  lat_limavh = 45.
  lat_limavh = 90.
  windsp_lim = 150.
  windsp_min = 4.0
  iFilter = WHERE(sceneamv.windv gt -150. and nwpamv.windv gt -150. and $
                  sceneamv.lat gt -90.1 and $
                  sceneamv.windpress ge plim[0] and $
                  sceneamv.windpress le plim[1] and $
                  ABS(sceneamv.lat) lt lat_limavh and $
                  nwpamv.windsp le windsp_lim and $
                  sceneamv.windsp ge windsp_min,$
                  niFilter, COMPLEMENT=iFilterc)
ENDIF


IF (niFilter eq 0) THEN BEGIN
  PRINT, 'No valid cases to plot.  Returning to main.'
  RETURN 
ENDIF 

;--- blacklisted data (emulate gsi/main/setupw.f90)
;--- no gross checks on data (no forecast input) are performed.
IF (gsiblacklist) THEN BEGIN
  itype  = REFORM(sceneamv.qc[*,1])
  zenith = REFORM(sceneamv.angle)
  presw  = sceneamv.windpress
  troppres = 0.0
  GSI_BLACKLIST, itype, zenith, presw, troppres, iuse
  ii = WHERE(iuse eq 1,nii)
  ; get the intersection of cases
  ; selected above and those blacklisted 
  iFilter  = INTERSECT(iFilter,ii)
  niFilter = N_ELEMENTS(iFilter)
  ; the complement of the accepted cases 
  ii = LINDGEN(N_ELEMENTS(sceneamv.windu))
  iFilterc  = SETDIFF(ii,iFilter)
  niFilterc = N_ELEMENTS(iFilterc)
ENDIF 

  xbinsize = 1.5
  ybinsize = 1.0
  qify  = REFORM(sceneamv.qi[iFilter,1])
  qifn  = REFORM(sceneamv.qi[iFilter,2])
  fcc   = REFORM(sceneamv.qi[iFilter,3])
  ytit     = 'Wind Speed (' + istr + ' - NWP), [m/s]'
  xtit     = 'QIFY, [1]'
  tit      = ''
  iplotStats  = 1
  chsz        = 1.2
  symbol_size = 2.0
  xrange = [0., 100.]
  yrange = [-70., 100.]
  test = 'dummy'
  Y = sceneamv.windsp[iFilter] - nwpamv.windsp[iFilter]
  psplotl=0
  !ORDER=0
  DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],qify,Y,$
                 xtit,ytit,title,chsz,iplotStats, stats, test + '.png', $
                 xbinsize,ybinsize,symbol_size, psplotl, /stats
  test = test + 'n'
  xtit     = 'QIFN, [1]'
  DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],qifn,Y,$
                 xtit,ytit,title,chsz,iplotStats, stats, test + '.png', $
                 xbinsize,ybinsize,symbol_size, psplotl, /stats

  test = test + 'f'
  xtit     = 'FCC, [1]'
  DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],fcc,Y,$
                 xtit,ytit,title,chsz,iplotStats, stats, test + '.png', $
                 xbinsize,ybinsize,symbol_size, psplotl, /stats
;  print, niFilter
  ; old - no QI => quality mark 
;  iFilter = WHERE(sceneamv.windv gt -150. and nwpamv.windv gt -150. and $
;              sceneamv.lat gt -90.1 and $
;              sceneamv.windpress ge plim[0] and $
;              sceneamv.windpress le plim[1] and qify gt 80.,niFilter, COMPLEMENT=iFilterc)
;  print, niFilter
 
  

;--- ecmwf QI list --- simplified 
IF (ecmwfqi) THEN BEGIN
  hl = [10., 400.]
  ml = [400.,700.]
  ll = [700., 1000.]
  itype  = REFORM(sceneamv.qc[*,1])
  qify  = REFORM(sceneamv.qi[*,1])
  qifn  = REFORM(sceneamv.qi[*,2])
  ; satellite ids 
  ; 257: GOES 13, 259: GOES 15, 
  ; 172: MTSAT-2R, 54: METEOSAT 7, 57: METEOSAT 10, 
  ; 783: TERRA, 784: AQUA 
  said = REFORM(sceneamv.qc[*,3])
  wcm  = sceneamv.windcalcmethod

  presw = sceneamv.windpress
  itrop = WHERE(ABS(sceneamv.lat) le 20., COMPLEMENT=iextrop)

  igoes = WHERE( (itype eq 245 and qifn ge 50) or $
                 (itype eq 251 and presw ge ll[0] and presw le ll[1] and $
                  qifn ge 50.) or $
                 (itype eq 246 and presw ge hl[0] and presw le hl[1]), ngoes)
  ieum1 = WHERE( said eq 54 and (itype eq 253 and qify ge 85.) or $
                (itype eq 243 and presw ge ll[0] and presw le ll[1] and $
                 qify ge 65.) or $
                (wcm eq 3 and itype eq 254 and presw ge hl[0] and presw le ml[1] and $
                 qify ge 80.), neum )
  ieum2 = WHERE( said eq 57 and (itype eq 253 and qifn ge 85.) or $
                (itype eq 243 and presw ge ll[0] and presw le ll[1] and $
                 qifn ge 85.) or $
                (wcm ge 3 and itype eq 254 and presw ge hl[0] and presw le ml[1] and $
                 qifn ge 85.), neum )
  ijma = WHERE( (itype eq 252 and qify ge 85.) or $
                (itype eq 242 and presw ge ll[0] and presw le ll[1] and $
                 qify ge 85.) or $
                (wcm eq 3 and itype eq 250 and presw ge hl[0] and presw le hl[1] and $
                 qify ge 85. ), njma )
  imod = WHERE(itype ge 257 and said eq 784, nmod) ; only AQUA winds assimilated
  iall = [igoes, ieum1, ieum2, ijma, imod]
  iunq = UNIQ(iall,SORT(iall))
  iall = iall[iunq]
  ii   = WHERE(iall gt 0, nii)
  IF (nii gt 0) THEN iall = iall[ii] ELSE RETURN 
  
  iFilter  = INTERSECT(iFilter,iall)
  niFilter = N_ELEMENTS(iFilter)
  ; the complement of the accepted cases 
  ii = LINDGEN(N_ELEMENTS(sceneamv.windu))
  iFilterc  = SETDIFF(ii,iFilter)
  niFilterc = N_ELEMENTS(iFilterc)

ENDIF 

;---build title strings
qcstr      = 'no QC'
IF (qualityMark) THEN qcstr = 'QC''d'
IF (ecmwfqi) THEN qcstr = 'ECMWF QI'
IF (gsiblacklist) THEN qcstr = qcstr + ' (BL)'
IF (highlevel) THEN qcstr = qcstr + ' High Level'
IF (lowlevel) THEN qcstr = qcstr + ' Low Level'

;qcstr = qcstr + ' QI > 80.'
title      = instrument + ' ' + qcstr + ' ' + date + ' t' + time + 'z'

;---find unique algorithm descriptors 
iunq = UNIQ(sceneamv.algdesc[iFilter],SORT(sceneamv.algdesc[iFilter]))
nunq = N_ELEMENTS(iunq)
adu  = sceneamv.algdesc[iFilter[iunq]]

;---setup taylor plot structure definition for windspeed
TAYLOR_DEF, taylor, nunq

;---setup taylor plot structure definition for wind direction
TAYLOR_DEF, taylor_ang, nunq

;---setup taylor plot structure definition for wind u
TAYLOR_DEF, taylor_u, nunq

;---setup taylor plot structure definition for wind v
TAYLOR_DEF, taylor_v, nunq

;---taylor setup tickmarks
majtick = [0., 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 1.]
mintick = [0., 0.5, 0.1, 0.15, 0.25, 0.3, 0.35, $
           0.45, 0.5, 0.55, 0.65, 0.7, 0.75, 0.85, $
           0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, $
           0.98, 0.99]
taylor.tayplot.cticks_maj = majtick
taylor.tayplot.cticks_min = mintick
taylor.tayplot.nctick_maj = N_ELEMENTS(majtick)
taylor.tayplot.nctick_min = N_ELEMENTS(mintick)

taylor.tayplot.colortable = 33L
taylor.tayplot.crange     = [0.2,0.8]
taylor.tayplot.crange     = [0.1,0.95]

taylor.tayplot.skillscore = 1L
taylor.correl_theory_max  = 0.75
taylor.correl_theory_max  = 0.89
taylor.tayplot.ytitle     = 'Normalized Wind Speed Standard Deviation, [1]'
taylor.tayplot.xtitle     = 'Normalized Wind Speed Standard Deviation, [1]'
taylor.tayplot.title      = title

;---copy structure information to structures for direction, u, and v winds
taylor_ang = taylor 
taylor_u = taylor 
taylor_v = taylor 
taylor_ang.tayplot.ytitle = 'Normalized Wind Direction Standard Deviation, [1]'
taylor_ang.tayplot.xtitle = 'Normalized Wind Direction Standard Deviation, [1]'

taylor_u.tayplot.ytitle = 'Normalized U-Wind Standard Deviation, [1]'
taylor_u.tayplot.xtitle = 'Normalized U-Wind Standard Deviation, [1]'

taylor_v.tayplot.ytitle = 'Normalized V-Wind Standard Deviation, [1]'
taylor_v.tayplot.xtitle = 'Normalized V-Wind Standard Deviation, [1]'

windspeed = 1
angle    = WINDANGLE(sceneamv.windu[iFilter],$
                     sceneamv.windv[iFilter],windspeed=windspeed)
anglenwp = WINDANGLE(nwpamv.windu[iFilter],$
                     nwpamv.windv[iFilter],windspeed=windspeednwp)
 
lat       = sceneamv.lat[iFilter]
lon       = sceneamv.lon[iFilter]

;---limits for checking wrap around effects at 0, 360. degrees
windlim = [90., 270.]
wp    = sceneamv.windpress[iFilter]
uamv  = sceneamv.windu[iFilter]
unwp  = nwpamv.windu[iFilter]
vamv  = sceneamv.windv[iFilter]
vnwp  = nwpamv.windv[iFilter]

;---pressures to plot wind statistics versus NWP
pres  = REVERSE([950., 850., 750., 650., 550., 450., 350., 250., 150.])
npres = N_ELEMENTS(pres)
dpres = 100.
;---index of the AMV wind pressures into the statistics array
ip    = LONG((wp - MIN(pres))/dpres + 0.49)

;---set up statistics arrays for wind speed, direction, u, v for type of
;   obs and all cases combined 
bias  = FLTARR(npres, nunq, 4)
rmse  = FLTARR(npres, nunq, 4)
nstt  = FLTARR(npres, nunq, 4)
biasa = FLTARR(npres, 4)
rmsea = FLTARR(npres, 4)
nstta = FLTARR(npres, 4)

;---loop over unique algorithms, build labels and accumulate statistics
FOR iu = 0L, nunq-1L DO BEGIN

    idu = WHERE(sceneamv.algdesc[iFilter] eq adu[iu],ndu)
;    PRINT, STRTRIM(adu[iu],2), ndu, FORMAT="(a,' Number of points: ',i7)" 
    str = STRING(STRTRIM(adu[iu],2), ndu, "(a,', ',i7)" )
    taylor.names[iu]   = str
    taylor_ang.names[iu]   = str
    taylor_u.names[iu]   = str
    taylor_v.names[iu]   = str
    IF (ndu lt 10) THEN continue 
    ipos = STRPOS(adu[iu],'winds')
    IF (ipos ne -1) THEN adu[iu] = STRMID(adu[iu],0,ipos-1)
    ipos = STRPOS(adu[iu],'cloud top')
    IF (ipos ne -1) THEN adu[iu] = STRMID(adu[iu],0,ipos-1) + ' ctp'
    ipos = STRPOS(adu[iu],'deep')
    IF (ipos ne -1) THEN adu[iu] = STRMID(adu[iu],0,ipos-1) + ' dlw'
    ipos = STRPOS(adu[iu],'Ch 4')
    IF (ipos ne -1) THEN adu[iu] = STRMID(adu[iu],0,ipos-1)  

    xrf = windspeednwp[idu]
    xrt = windspeed[idu]

    corra  = CORRELATE(xrf,xrt)
    
    taylor.refstd[iu]  = SQRT(VARIANCE(xrf))
    taylor.retstd[iu]  = SQRT(VARIANCE(xrt))
    taylor.correl[iu]  = corra[0]

    xrf = anglenwp[idu]
    xrt = angle[idu]
    iFilteran  = WHERE(xrf ge windlim[1] and xrt le windlim[0],niFilteran)
    IF (niFilteran gt 0) THEN xrf[iFilteran] -= 360.0
    iFilteran  = WHERE(xrt ge windlim[1] and xrf le windlim[0],niFilteran)
    IF (niFilteran gt 0) THEN xrt[iFilteran] -= 360.0
    corra  = CORRELATE(xrf,xrt)
    taylor_ang.refstd[iu]  = SQRT(VARIANCE(xrf))
    taylor_ang.retstd[iu]  = SQRT(VARIANCE(xrt))
    taylor_ang.correl[iu]  = corra[0]
    
    ;---angle 
    arf = xrf[idu]
    art = xrt[idu]
    ;---speed 
    xrf = windspeednwp[idu]
    xrt = windspeed[idu]
    ;---v-wind
    vrf = vnwp[idu]
    vrt = vamv[idu]
    ;---u-wind
    urf = unwp[idu]
    urt = uamv[idu]

    ;---accumulate statistics for vertical stats
    ipu = ip[idu]
    FOR ip0 = 0L, npres-1L DO BEGIN
       ip1 = WHERE(ipu eq ip0,nip1)
       IF (nip1 lt 2) THEN continue 
       bias[ip0, iu, 0] = TOTAL(xrt[ip1]-xrf[ip1])
       rmse[ip0, iu, 0] = TOTAL((xrt[ip1]-xrf[ip1])^2.)
       bias[ip0, iu, 1] = TOTAL(urt[ip1]-urf[ip1])
       rmse[ip0, iu, 1] = TOTAL((urt[ip1]-urf[ip1])^2.)
       bias[ip0, iu, 2] = TOTAL(vrt[ip1]-vrf[ip1])
       rmse[ip0, iu, 2] = TOTAL((vrt[ip1]-vrf[ip1])^2.)

       bias[ip0, iu, 3] = TOTAL(art[ip1]-arf[ip1])
       rmse[ip0, iu, 3] = TOTAL((art[ip1]-arf[ip1])^2.)

       biasa[ip0, 0] += TOTAL(xrt[ip1]-xrf[ip1])
       rmsea[ip0, 0] += TOTAL((xrt[ip1]-xrf[ip1])^2.)
       biasa[ip0, 1] += TOTAL(urt[ip1]-urf[ip1])
       rmsea[ip0, 1] += TOTAL((urt[ip1]-urf[ip1])^2.)
       biasa[ip0, 2] += TOTAL(vrt[ip1]-vrf[ip1])
       rmsea[ip0, 2] += TOTAL((vrt[ip1]-vrf[ip1])^2.)

       biasa[ip0, 3] += TOTAL(art[ip1]-arf[ip1])
       rmsea[ip0, 3] += TOTAL((art[ip1]-arf[ip1])^2.)

       nstt[ip0, iu, *] = nip1
    ENDFOR
       
    xrf = sceneamv.windu[iFilter[idu]]
    xrt = nwpamv.windu[iFilter[idu]]

    corra  = CORRELATE(xrf,xrt)
    
    taylor_u.refstd[iu]  = SQRT(VARIANCE(xrf))
    taylor_u.retstd[iu]  = SQRT(VARIANCE(xrt))
    taylor_u.correl[iu]  = corra[0]

    xrf = sceneamv.windv[iFilter[idu]]
    xrt = nwpamv.windv[iFilter[idu]]

    corra  = CORRELATE(xrf,xrt)
    
    taylor_v.refstd[iu]  = SQRT(VARIANCE(xrf))
    taylor_v.retstd[iu]  = SQRT(VARIANCE(xrt))
    taylor_v.correl[iu]  = corra[0]
    
ENDFOR 

;---create vertical statistics per obs type and all cases 
img_name =figPath + figStr + '_vstats'
bias = bias / nstt
mse  = rmse / nstt 
rmse = SQRT(mse)
stdv = SQRT(mse - bias*bias)

biasa = biasa / TOTAL(nstt,2)
msea  = rmsea / TOTAL(nstt,2)
rmsea = SQRT(msea)
stdva = SQRT(msea - biasa*biasa)

xrax  = [-6., 12.]
xraxa = [-35., 60.]
yrax  = [1000., 100.]
strs  = ['Wind Speed', 'U-Wind', 'V-Wind', 'Wind Direction']
imstr = ['ws', 'wu', 'wv', 'wd']

FOR ist = 0L, 3L DO BEGIN
  if (psplot eq 0) then begin
    set_plot,'Z'
    device,set_resolution=[650, 500]
  endif 
  if (psplot eq 1) then begin
    xz=18 & yz=18
    close,/all
    erase
    !p.multi=1
    !p.font=0
    set_plot,'ps'
;    !P.THICK = 2
    device,filename=img_name + imstr[ist] + '.ps',/color,$
           bits_per_pixel=8,xsize=xz,ysize=yz,$
           xoffset=1,yoffset=1,/encapsulated,$
           /portrait,font_size=13,/courier
  endif
  bottom=3
  colors = LINDGEN(nunq)+bottom
  LOADCT, 39, NCOLORS=nunq+1, BOTTOM=bottom
  TVLCT, rr, gg, bb, /get
  rr(0)=255   & gg(0)=255   & bb(0)=255 ; Load White Color - 0 for background 
  rr(255)=0   & gg(255)=0   & bb(255)=0 ; Load Black Color - 255
  nc=!D.table_size
  nc=nc-2
  unit = '[m/s]'
  IF (ist eq 3) THEN BEGIN
     xrax = xraxa
     unit = '[deg.]'
  ENDIF   
  PLOT_IO, xrax, yrax, XRANGE=xrax, YRANGE=yrax, /NODATA, $
           XTITLE=strs[ist] + ' Bias and Std. Dev versus ' + nwpstr + ', '  + unit, $
           YTITLE='Pressure, [hPa]', CHARSIZE=1.2, $
           TITLE=title, COLOR=bottom ; black
  OPLOT, [0., 0.], yrax, COLOR=bottom, LINESTYLE=2
  FOR iu = 0L, nunq-1L DO BEGIN
    OPLOT, bias[*,iu,ist], pres, COLOR=colors[iu], PSYM=-4, SYMSIZE=0.5
    OPLOT, stdv[*,iu,ist], pres, COLOR=colors[iu], LINESTYLE=2
  ENDFOR  
  OPLOT, biasa[*,ist], pres, COLOR=bottom, SYMSIZE=0.5, THICK=4, LINESTYLE=3
  OPLOT, stdva[*,ist], pres, COLOR=bottom, SYMSIZE=0.5, THICK=4, LINESTYLE=3
  linestyles = LONARR(nunq+1)
  linestyles[nunq] = 3
;  LOADCT, 39, NCOLORS=nunq+1, BOTTOM=bottom
  colorsl = [LINDGEN(nunq)+bottom, bottom]
  adul    = [adu, 'All']
  EMLEGEND, xrax, yrax, adul, colorsl, linestyles, xlog=xlogx, $
          ylog=1, pos=posx, charsize=0.8, dpos=.35, $
          center=centerx, psym=4, xpos=0.175, ypos=0.968, textcolor=bottom
  IF (psplot eq 0) THEN $
     WRITE_PNG, img_name + imstr[ist] + '.png', TVRD(), rr, gg, bb, order=order
  DEVICE, /close
ENDFOR

;---scatter plots of wind speed, u, and v for different algorithm types
adu  = sceneamv.algdesc[iFilter[iunq]]
xrax = [0., 120.]
xmax = 120.
IF (STRTRIM(STRMID(instrument,0,3),2) eq 'ISS') THEN BEGIN
  xmax = 35.0
ENDIF
xrax = [0., xmax]
yrax = xrax 
ytitle = ' Wind Speed, [m/s]'
xtitle = nwpstr + ' Wind Speed, [m/s]'
xret = windspeed 
xref = windspeednwp 
img_name =figPath + figStr + '_wsscm'+figSuf
algdesc = sceneamv.algdesc[iFilter]
ScatterMultiple, nunq, algdesc, adu, xref, xret, xtitle, ytitle, title, $
  xrax, yrax, img_name, psplot, order=order, font_size=fontsize

xrax = [-30., 150.]
yrax = xrax 
ytitle = ' U-Wind, [m/s]'
xtitle = nwpstr + ' U-Wind, [m/s]'
xret = sceneamv.windu[iFilter] 
xref = nwpamv.windu[iFilter] 
img_name =figPath + figStr + '_wuscm'+figSuf
ScatterMultiple, nunq, algdesc, adu, xref, xret, xtitle, ytitle, title, $
  xrax, yrax, img_name, psplot, order=order, font_size=fontsize

xrax = [-60., 150.]
yrax = xrax 
ytitle = ' V-Wind, [m/s]'
xtitle = nwpstr + ' V-Wind, [m/s]'
xret = sceneamv.windv[iFilter] 
xref = nwpamv.windv[iFilter] 
img_name =figPath + figStr + '_wvscm'+ figSuf
ScatterMultiple, nunq, algdesc, adu, xref, xret, xtitle, ytitle, title, $
  xrax, yrax, img_name, psplot, order=order, font_size=fontsize


;---------------------------------------------------------------
;---need to add more routines to library to run taylor_plot.pro.
;---turn off for now. 
;; TAYLOR_PLOT, taylor, /legend, save=modtayx
;; TAYLOR_PLOT, taylor_ang, /legend, save=modtayx
;; TAYLOR_PLOT, taylor_u, /legend, save=modtayx
;; TAYLOR_PLOT, taylor_v, /legend, save=modtayx
;; TPLOT, /OFF

;---create density plot figures for various inputs versus inputs 
x = windspeednwp
y = windspeed
xbinsize = .5
ybinsize = .5
yrange   = [0., xmax]
xrange   = [0., xmax]
xtit     = nwpstr + ' Wind Speed, [m/s]'
ytit     = istr + ' Wind Speed, [m/s]'
tit      = ''
iplotStats  = 1
chsz        = 1.2
symbol_size = 2.0
gridscale = xbinsize
DensityScatter,xrange[0],xrange[1],xrange[0],xrange[1],X,Y,$
               xtit,ytit,title,chsz,iplotStats, stats,figPath + $
               figStr + '_dd'+ figSuf, $
               gridScale,symbol_size, psplot

;--- NOTE: Not all satellites/algorithm have prepobs_errtable information
winderror = sceneamv.winderror[iFilter]
iid = WHERE(winderror le 12,niid)
IF (niid ne 0) THEN BEGIN
  x = windspeed[iid]
  y = winderror[iid]

  yrange   = [0., 10.]
  xrange   = [0., xmax]
  xbinsize = 1.5
  ybinsize = .25
  xtit     = istr + ' Wind Speed, [m/s]'
  ytit     = istr + ' Wind Error, [m/s]'
  DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
              xtit,ytit,title,chsz,iplotStats,stats,figPath + figStr + '_wews'+ figSuf, $
              xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize
ENDIF 

x = windspeednwp
xtit     = nwpstr + ' Wind Speed, [m/s]'
x = windspeed
xtit     = 'Wind Speed, [m/s]'

y = (windspeed-windspeednwp)
nlevels    = 20
xbinsize = 2.5
ybinsize = 1.0
yrange     = [-15., 15.]
xrange     = [0., 35.]
xrange     = [0., xmax]
ytit     = istr + ' - ' + nwpstr + ' Wind Speed, [m/s]'
iFilterl = WHERE(x ge xrange[0] and x le xrange[1] and $
            y ge yrange[0] and y le yrange[1])
DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtit,ytit,title,chsz,iplotStats,stats,figPath + figStr + '_wswsd'+ figSuf, $
  xbinsize,ybinsize, symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize

;; DensityPlotContour,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
;;   xtit,ytit,title,chsz,iplotStats,stats,figPath + figStr + '_wswsd.'+ figSuf, $
;;   xbinsize,ybinsize, symbol_size, 0, /stats, statpos=2, order=order, font_size=fontsize


x = sceneamv.windPress[iFilter]
y = (windspeed-windspeednwp)
xbinsize = 50.0
ybinsize = 1.0
yrange     = [-15., 15.]
xrange     = [50., 1000.]
xtitle     = istr + ' Wind Pressure, [hPa]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Speed, [m/s]'
;DensityPlotContour,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
;  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wpwsd'+ figSuf, $
;  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize

DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wpwsd'+ figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize


x = sceneamv.windPress[iFilter]
y = (angle-anglenwp)
iFilterang = WHERE(anglenwp ge windlim[1] and angle le windlim[0],niFilterang)
IF (niFilterang gt 0) THEN y[iFilterang] = (angle[iFilterang] - (anglenwp[iFilterang] - 360.))
iFilterang = WHERE(angle ge windlim[1] and anglenwp le windlim[0],niFilterang)
IF (niFilterang gt 0) THEN y[iFilterang] = (angle[iFilterang]-360. - anglenwp[iFilterang])
xbinsize = 50.0
ybinsize = 2.0
yrange     = [-50., 50.]
xrange     = [50., 1100.]
xtitle     = istr + ' Wind Pressure, [hPa]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Direction, [deg.]'
DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
            xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wpwdd'+ figSuf, $
            xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize

; DensityPlotContour,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
;                    xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wpwdd'+ figSuf, $
;                    xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize

x = winderror
y = (windspeed-windspeednwp)
xbinsize = 1.0
ybinsize = 1.0
yrange     = [-15., 15.]
xrange     = [0., 10.]
xtitle     = istr + ' Wind Error, [m/s]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Speed, [m/s]'
DensityPlotContour,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wewsd'+ figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize


x = lat
y = (windspeed-windspeednwp)
xbinsize = 8.0
ybinsize = 1.0
yrange     = [-15., 15.]
xrange     = [-90., 90.]
xtitle     = istr + ' Latitude, [deg.]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Speed, [m/s]'
DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wltwsd'+ figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize


x = sceneamv.angle[iFilter]
y = (windspeed-windspeednwp)
xbinsize = 10.0
ybinsize = 1.0
yrange     = [-15., 15.]
xrange     = [-90., 90.]
xtitle     = istr + ' Wind Zenith Angle, [deg.]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Speed, [m/s]'
DensityPlotContour,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wzwsd'+ figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize


x = windspeednwp
y = (angle-anglenwp)
iFilterang = WHERE(anglenwp ge windlim[1] and angle le windlim[0],niFilterang)
IF (niFilterang gt 0) THEN y[iFilterang] = (angle[iFilterang] - (anglenwp[iFilterang] - 360.))
iFilterang = WHERE(angle ge windlim[1] and anglenwp le windlim[0],niFilterang)
IF (niFilterang gt 0) THEN y[iFilterang] = (angle[iFilterang]-360. - anglenwp[iFilterang])
a1 = angle MOD 360.
a2 = anglenwp MOD 360.
xbinsize = 1.0
ybinsize = 2.0
yrange     = [-50., 50.]
xrange     = [0., 35.]
xtitle     = nwpstr + ' Wind Speed, [m/s.]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Direction, [deg.]'
DensityPlotContour,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wswdd'+ figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize


x = anglenwp
y = (angle-anglenwp)
iFilterang = WHERE(anglenwp ge windlim[1] and angle le windlim[0],niFilterang)
IF (niFilterang gt 0) THEN y[iFilterang] = (angle[iFilterang] - (anglenwp[iFilterang] - 360.))
iFilterang = WHERE(angle ge windlim[1] and anglenwp le windlim[0],niFilterang)
IF (niFilterang gt 0) THEN y[iFilterang] = (angle[iFilterang]-360. - anglenwp[iFilterang])

xbinsize = 10.0
ybinsize = 5.0
yrange     = [-180., 180.]
xrange     = [0., 360.]
xtitle     = nwpstr + ' Wind Direction, [deg.]'
ytitle     = istr + ' - ' + nwpstr + ' Wind Direction, [deg.]'
DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wdwdd' + figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize

x = nwpamv.windu[iFilter]
y = sceneamv.windu[iFilter]-nwpamv.windu[iFilter]
xbinsize = 2.5
ybinsize = 1.0
yrange     = [-20., 20.]
xrange     = [-35., 80.]
xrange     = [-xmax/2., xmax/2.]
xtitle     = nwpstr + ' Wind U, [m/s]'
ytitle     = istr + ' - ' + nwpstr + ' Wind U, [m/s]'
DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wuwud' + figSuf, $
  xbinsize, ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize


x = nwpamv.windv[iFilter]
y = sceneamv.windv[iFilter]-nwpamv.windv[iFilter]
xbinsize = 2.5
ybinsize = 1.0
yrange     = [-20., 20.]
xrange     = [-60., 60.]
xrange     = [-xmax/2., xmax/2.]
xtitle     = nwpstr + ' Wind V, [m/s]'
ytitle     = istr + ' - ' + nwpstr + ' Wind V, [m/s]'
DensityPlot,xrange[0],xrange[1],yrange[0],yrange[1],X,Y,$
  xtitle,ytitle,title,chsz,iplotStats,stats,figPath + figStr + '_wvwvd' + figSuf, $
  xbinsize,ybinsize,symbol_size, psplot, /stats, statpos=2, order=order, font_size=fontsize
;---finished with density plots 

lat       = sceneamv.lat[iFilter]
lon       = sceneamv.lon[iFilter]
minvalue = 0.
maxvalue = 75.
div      = 10
position = [0.15, 0.20, 0.95, 0.88]
title      = qcstr + ' ' + date + ' t' + time + 'z'
title2 = title + ' Wind Speed, [m/s]'
img_name = figPath + figstr + '_map.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            sceneamv.Windsp,'Wind Speed, [m/s]',0.6,8,1,0,'(f7.2)',8,img_name,$
            color_table_index = 33

title2 = title + ' Wind Speed, [m/s]'
img_name = figPath + figstr + '_mapnwp.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            nwpamv.Windsp,'Wind Speed, [m/s]',0.6,8,1,0,'(f7.2)',8,img_name,$
            color_table_index = 33

minvalue = -15.
maxvalue =  15.
title2 = title + ' ' + istr + '  - ' + nwpstr + ' Wind Speed, [m/s]'
img_name = figPath + figstr + '_mapwsd.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            sceneamv.Windsp-nwpamv.Windsp,'Wind Speed, [m/s]',0.6,8,1,0,'(f7.2)',8,img_name,$
            color_table_index = 33


minvalue = -25.
maxvalue =  25.
title2 = title + ' ' + istr + '  - ' + nwpstr + ' U-Wind Speed, [m/s]'
img_name = figPath + figstr + '_mapuwd.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            sceneamv.WindU-nwpamv.WindU,'Wind Speed, [m/s]',0.6,8,1,0,'(f7.2)',8,img_name,$
            color_table_index = 33

minvalue = -25.
maxvalue =  25.
title2 = title + ' ' + istr + '  - ' + nwpstr + ' V-Wind Speed, [m/s]'
img_name = figPath + figstr + '_mapvwd.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            sceneamv.WindV-nwpamv.WindV,'Wind Speed, [m/s]',0.6,8,1,0,'(f7.2)',8,img_name,$
            color_table_index = 33

minvalue = -180.
maxvalue =  180.
title2 = title + ' Wind Direction, [deg.]'
img_name = figPath + figstr + '_mapwd.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            sceneamv.Winddir,'Wind Dir., [deg.]',0.6,8,1,0,'(f8.1)',8,img_name,$
            color_table_index = 33

title2 = title + ' ' + istr + ' - ' + nwpstr + ' Wind Direction, [deg.]'
minvalue = -90.
maxvalue =  90.
img_name = figPath + figstr + '_mapwdd.png'
mapPlot_png,-90,90,-180,180,sceneamv.lat,sceneamv.lon,iFilter,title2,minValue,maxValue,$
            sceneamv.Winddir-nwpamv.Winddir,'Wind Direction, [deg]',0.6,8,1,0,'(f8.1)',8,img_name,$
            color_table_index = 33


return
;---issue with QC'd data in the following plotting section ---need to remove from SVN
;version - ESM 02/25/2015

;---latitude and longitude bins for gridded observation maps 
dlat = 1.0
lat0 = -90. & lat1 = 90. & nlat = LONG((lat1-lat0)/dlat + 1)
lats = lat0 + dlat*FINDGEN(nlat)

dlon = 1.0
lon0 = -180. & lon1 = 180. & nlon = LONG((lon1-lon0)/dlon + 1)
lons = lon0 + dlon*FINDGEN(nlon)

;---create gridded values of wind speed and direction for map figures
windsp = sceneamv.windsp[iFilter]
var  = FLTARR(nlon, nlat) 
vare = FLTARR(nlon, nlat) 
;---u, v winds 
var2  = FLTARR(nlon, nlat,2) 
var2e = FLTARR(nlon, nlat,2) 
nvr   = FLTARR(nlon, nlat)
xtit  = ''
ytit  = ''
x     = lon
y     = lat
satwindsp = sceneamv.windsp[iFilter]
nwpwindsp = nwpamv.windsp[iFilter]

satwindu = sceneamv.windu[iFilter]
nwpwindu = nwpamv.windu[iFilter]
satwindv = sceneamv.windv[iFilter]
nwpwindv = nwpamv.windv[iFilter]

; 
ilt   = LONG((y - lat0)/dlat + 0.49)
iln   = LONG((x - lon0)/dlon + 0.49)
FOR ii = 0L, niFilter-1L DO BEGIN
   ix = iln[ii]
   iy = ilt[ii]
   IF (ix ge nlon) THEN ix = nlon-1
   IF (iy ge nlat) THEN iy = nlat-1
   IF (ix lt 0 or iy lt 0) THEN continue 
   var[ix,iy]  += satwindsp[ii]
   vare[ix,iy] += nwpwindsp[ii]

   var2[ix,iy,0]  += satwindu[ii]
   var2e[ix,iy,0] += nwpwindu[ii]
   var2[ix,iy,1]  += satwindv[ii]
   var2e[ix,iy,1] += nwpwindv[ii]
   nvr[ix,iy] += 1
ENDFOR 
;---cases where QC or BL failed 
ilt = LONG((sceneamv.lat[iFilterc] - lat0)/dlat + 0.49)
iln = LONG((sceneamv.lon[iFilterc] - lon0)/dlon + 0.49)
FOR ii = 0L, N_ELEMENTS(iFilterc)-1L DO BEGIN
   ix = iln[ii]
   iy = ilt[ii]
   IF (ix ge nlon) THEN ix = nlon-1
   IF (iy ge nlat) THEN iy = nlat-1
   IF (ix lt 0 or iy lt 0) THEN continue 
   IF (nvr[ix,iy] gt 1.) THEN continue 
   var[ix,iy]   = 990.
   vare[ix,iy]  = 990.
   var2[ix,iy,*]  = 990.
   var2e[ix,iy,*] = 990.
   nvr[ix,iy] = -10
ENDFOR 

var  = var/nvr 
vare = vare/nvr 

iFilterb2 = WHERE(var2 eq 0,niFilterb)
var2[*,*,0] = var2[*,*,0]/nvr 
var2[*,*,1] = var2[*,*,1]/nvr 
var2e[*,*,0] = var2e[*,*,0]/nvr 
var2e[*,*,1] = var2e[*,*,1]/nvr 
iFilterb = WHERE(nvr eq 0,niFilterb)
IF (niFilterb ne 0) THEN var[iFilterb] = -999.
IF (niFilterb ne 0) THEN vare[iFilterb] = -999.
IF (niFilterb ne 0) THEN var2[iFilterb2] = -999.
IF (niFilterb ne 0) THEN var2e[iFilterb2] = -999.

dvar  = var-vare

var2a    = WINDANGLE(var2[*,*,0],$
                     var2[*,*,1])

var2ea   = WINDANGLE(var2e[*,*,0],$
                     var2e[*,*,1])
ida = WHERE(var2a lt 0.,nida)
IF (nida ne 0) THEN var2a[ida] += 360.0

ida = WHERE(var2ea lt 0.,nida)
IF (nida ne 0) THEN var2ea[ida] += 360.0

dvar2 = var2a-var2ea

ib  = WHERE(var2[*,*,0] eq -99.,nb)
IF (nb ne 0) THEN dvar2[ib] = 998.
ib  = WHERE(var2[*,*,0] eq -99.,nb)
IF (nb ne 0) THEN var2a[ib] = -99.
ib  = WHERE(var2[*,*,0] eq -999.,nb)
IF (nb ne 0) THEN dvar2[ib] = -999.
ib  = WHERE(var2[*,*,0] eq -999.,nb)
IF (nb ne 0) THEN var2a[ib] = -999.

ib  = WHERE(var eq -99.,nb)
IF (nb ne 0) THEN dvar[ib]  = -99.
ib  = WHERE(var eq -999.,nb)
IF (nb ne 0) THEN dvar[ib]  = -999.

; correct angle differences near 0.0 / 360. deg.
iFilterang = WHERE((var2ea ge windlim[1] and var2ea le 360.0) and (var2a ge 0. and var2a le windlim[0]),niFilterang)
IF (niFilterang gt 0) THEN dvar2[iFilterang] = (var2a[iFilterang] - (var2ea[iFilterang] - 360.))
iFilterang = WHERE((var2a ge windlim[1] and var2a le 360.0) and (var2ea ge 0. and var2ea le windlim[0]),niFilterang)
IF (niFilterang gt 0) THEN dvar2[iFilterang] = (var2a[iFilterang]-360. - var2ea[iFilterang])

minvalue = 0.
maxvalue = 75.
div      = 10
position = [0.15, 0.20, 0.95, 0.88]
sfc = var - var
title2 = title + ' Wind Speed, [m/s]'
PLOT_GRID,var,sfc,figPath + figstr + '_map.png',minvalue,maxvalue,lat0,lat1,$
          lon0,lon1,title2,-1,div,format,$
          color_table_index=33, order=order

title2 = nwpstr + ' Wind Speed, [m/s]'
PLOT_GRID,vare,sfc,figPath + figstr + '_mapnwp.png',minvalue,maxvalue,lat0,lat1,$
          lon0,lon1,title2,-1,div,format,$
          color_table_index=33, order=order

minvalue = -15.
maxvalue =  15.
title2 = title + istr + '  - ' + nwpstr + ' Wind Speed, [m/s]'
PLOT_GRID,dvar,sfc,figPath + figstr + '_mapwsd.png',minvalue,maxvalue,lat0,lat1,lon0,lon1,title2,-1,div,format,$
	      color_table_index=33, order=order

minvalue = 0.
maxvalue = 360.
title2 = title + ' Wind Direction, [deg.]'
PLOT_GRID,var2a,sfc,figPath + figstr + '_mapwd.png',minvalue,maxvalue,lat0,lat1,lon0,lon1,title2,-1,div,format,$
	      color_table_index=33, order=order

title2 = title + ' ' + istr + ' - ' + nwpstr + ' Wind Direction, [deg.]'
minvalue = -30.
maxvalue = 30.
PLOT_GRID,dvar2,sfc,figPath + figstr + '_mapwdd.png',minvalue,maxvalue,lat0,lat1,lon0,lon1,title2,-1,div,format,$
	      color_table_index=33, order=order, qcfail_val=998.

; end plotting 
RETURN 
   
END 
