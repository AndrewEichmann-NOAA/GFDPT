PRO HSIR_FILTER, sensorID, tb, qc, sfctypevec, tskinvec, solangle, angle, $
                 sfctype, latitude, iclear, nclear

;===============================================================
;+
; Name:       HSIR_FILTER
;
; Type:       IDL Subroutine
;
; Description: Compute empirically derived skin temperature from
;              hyperspectral IR measurements (AIRS, IASI, CrIS) or 
;              broadband IR measurements (AHI, ABI, GOES Imager, ..).
;              and use QC (spatial-homogeneity tests) to find
;              spatially uniform warm and empirical surface
;              temperature close to NWP --> clear cases.
;
;              Coefficients trained using ir_sstreg.pro
;
; Arguments:
;      Name      Dim      Type             Description
;      ---------------------------------------------------
;      -sensorID          Input            sensorID character string
;      -tb                Input            Brightness temperature all channels
;      -qc                Input            QC from rad file
;      -sfctypevec        Input            surface type from NWP 
;                                          collocation/classification
;      -tskinvec          Input            NWP surface skin
;                                          temperature
;      -solangle          Input            Solar zenith angle
;      -angle             Input            Sensor zenith angle
;      -sfctype           Input            Coefficients for this
;                                          surface type
;      -iclear           Output            Index of "clear-sky" cases
;      -nclear           Output            Number of "clear-sky" cases
;
; History:  
;        10-28-2014    Eric S. Maddy, Rti @ NOAA/NESDIS/STAR/JCSDA
;                      Created/modified for use within the COAT.
;        02-02-2015    Eric S. Maddy, added AHI 
;
; Limitations: 
;    Works only for ocean; however, because the channels selected
;    for the regression are spectrally-close together, the 
;    regression should work "ok" for non-ocean surface.  
;-
;===============================================================

  IF (sfctype ne 0) THEN BEGIN
    PRINT, ' Not a valid surface type ... no clear indices'
    igood = -1
    ngood = 0
  ENDIF
  ;---temperature at which water freezes (about)
  tfreeze     = 273.0
  ;---latitude limits for regressions
  lat_lim     = [-45., 45.]     ; no high latitude
  ;---solar angle limits -- not now.
  solzang_min = 95.
  ;---zenith angle limits

  zenang_max = 50.

  ; --------------------------------------
  ;--- AHI shows significant deviations
  ;     from CRTM at angles > 50 deg.  
  ; ---------------------------------------


  ;---determine coefficients based on sensor ID
  CASE sensorID OF
     'AHI': BEGIN  ; 399
       c0    = -51.0104
       coef  = [1.16778, -1.27133, 0.416716, 2.16380]
       ichan = [8,9]-1L
       dts_thresh = 1.0
       qc2_thresh = -1.           ; < coh_med 
     END
     'CRIS': BEGIN  ; 399
       c0    = -14.9263
       coef  = [1.05900, -0.719571, 0.263446, 2.37502]
       ichan = [186,188]-1L
       dts_thresh = 1.25
       qc2_thresh = 1           ; < coh_med 
     END
     'AIRS': BEGIN ; 281
       c0    = 2.12625
       coef  = [1.00438, -0.614952, 0.0945729, -0.692123]
       ichan = [166,165]-1
       dts_thresh = 1.25
       qc2_thresh = 1           ; < coh_med 
     END
     'IASI': BEGIN ; 616
       PRINT, ' No valid coefficients yet'
       stop
       c0 = -14.9263
       coef = [1.05900, -0.719571, 0.263446, 2.37502]
       ichan = [186,188]-1L
       dts_thresh = 1.25
       qc2_thresh = 1           ; < coh_med 
     END
     ELSE: BEGIN
       PRINT, ' Not a valid sensorID Option...stopping'
       stop
     END
  ENDCASE
  ;---compute secant of zenith angle
  seca = 1.0 / COS( angle * !pi/180. ) 

  ;---brightness temperature of channels used for regression
  bt   = REFORM(tb[*,ichan])

  ;---difference in BTs water-window or window-water 
  ;  (depends on channel selection)
  dbt  = REFORM(bt[*,1]-bt[*,0])

  ;---calculate regression surface temperature using 
  ;   empirical relation
  ts   = c0 + coef[0]*REFORM(bt[*,1]) + coef[1]*dbt + $
         coef[2]*dbt*dbt + coef[3] * seca
  IF (qc2_thresh gt 0) THEN BEGIN
    qc1  = REFORM(qc[*,1])  ; (FOR(MAX)-FOR(MIN))
    qc2  = REFORM(qc[*,3])  ; (FOR(MAX)-FOV(5))
  ENDIF ELSE BEGIN
    ;---file these variables with zeros for now
    qc1  = REFORM(qc[*,0]-qc[*,0])  ; (FOR(MAX)-FOR(MIN))
    qc2  = REFORM(qc[*,0]-qc[*,0])  ; (FOR(MAX)-FOV(5))
    qc2_thresh = 100
  ENDELSE
  ;---difference in retrieved regression SST and NWP SST 
  dts  = ts - tskinvec
  igood = WHERE(sfctypevec eq sfctype and $
                ts gt tfreeze and tskinvec gt tfreeze and $
                solangle gt solzang_min and $
                (latitude ge lat_lim[0] and latitude le lat_lim[1]) and $
                ABS(angle) le zenang_max,ngood)
  ;---center the distribution about the peak of the PDF for
  ;   ocean scenes
  IF (ngood gt 0) THEN BEGIN
    nbins = 1000.
    res1=HISTOGRAM(dts[igood],NBINS=nbins,LOCATIONS=loc1)
    res1=res1/float(max(res1))*100.
    ind=where(res1 ge 0.)
  
    mxr = MAX(res1,ipk)
    dts2 = dts - loc1[ipk[0]]
  ENDIF ELSE dts2 = dts
  ;---cases with low contrast over FOR(MAX) - FOV(5) (e.g., 
  ;   current FOV is the warmest or nearly the warmest) 
  ;   dts2 < dts_thresh and retrieved surface temperature > 0.
  iclear = WHERE(sfctypevec eq sfctype and $
                 ts gt tfreeze and $
                 ABS(dts2) le dts_thresh and $
                 tskinvec gt tfreeze and $
                 qc2 le qc2_thresh and $
                 solangle gt solzang_min and $
                 (latitude ge lat_lim[0] and latitude le lat_lim[1]) and $
                 ABS(angle) le zenang_max, nclear)
  
END
