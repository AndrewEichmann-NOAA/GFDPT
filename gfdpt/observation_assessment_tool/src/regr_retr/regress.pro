;===================================================================
;
; Name: regress.pro
;
; Language: IDL
;
; Programming Unit: Module
;
; Procedures contained:
;   - retrieve_CLW
;   - apply_regress
;
; Description:
;   - This IDL module contains procedures related to applying
;     regression algorithms to retrieve geophysical parameters
;     from a provided set of brightness temperature observations 
;     or simulations for various sensors.  It is expected that
;     the coefficients derived come from the 
;     mirs_utilities/src/idl/genRegression/GenerateRegressAlgors.pro
;     program.
;
; Author:
;   - Kevin Garrett, RTi @ NOAA/NESDIS/STAR/JCSDA
;
; History:
;   - 4/25/2014: Added retrieve_CLW and ApplyRegr procedures
;
;
;====================================================================


;===================================================================
;
; Name: retrieve_edr
;
; Language: IDL
;
; Programming Unit: Procedure
;
; Subroutines called:
;   - io_regressAlgors
;   - ApplyRegr
;
; Description:
;   - This IDL Procedure reads in regression coefficients from 
;     specified files, and calls the regression algorithm to 
;     apply brightness temperatures to the coefficients to retrieve
;     Cloud Liquid Water
;
; Arguments/Type/Description
;     Param      / Input    / String specifying parameter to retrieve
;     Tb         / Input    / Array of brightness temperatures (nChan/nProfiles)
;     Tskin      / Input    / Model TSKIN for emissivity (em2) retrieval
;     SfcTyp     / Input    / Array of surface type class (nProfiles)
;     coeff_path / Input    / Path the sensor regression coefficient files
;     Sensor_id  / Input    / String sensor id, e.g. 'amsr2'
;     angle      / Input    / Array of angle information (nProfiles)
;     scanpos    / Input    / Array of scan positions (nProfiles)
;     lat        / Input    / Array of latitude (nProfiles)
;     edr        / Output   / Return array of retrieved parameter
;
;
; Author:
;   - Kevin Garrett, RTi @ NOAA/NESDIS/STAR/JCSDA
;
; History:
;   - 4/25/2014: Initial code
;   - 4/01/2015: K. Garrett/E.S. Maddy consolidated changes 
;                for emissivity regressions
;
;====================================================================


PRO retrieve_edr,Param,Tb,Tskin,SfcTyp,coeff_path,Sensor_id,angle,scanpos,lat,edr

   ;---Assign parameters not based on arguments
   nAlgors=4                                 ;---Number of regression algorithms (usually = number of surface types
   mxIndepVar=26                             ;---Number of independent variables trained on
   nPar=200                                  ;---Number of parameter layers (Cloud is single layer)
;   nPar=1                                    ;---Number of parameter layers (Cloud is single layer)
   AlgorLabels=['Oc','SeaIce','Land','Snow'] ;---Corresponding to nAlgors and coeff file names
   iSfcTyp = indgen(nAlgors)                 ;---Surface type index 0-Ocean, 1-SeaIce, 2-Land, 3-Snow
   nchan2UseCLW=intarr(nAlgors)
   isConical=0
   isCrossTrack=0
   if (Sensor_id eq 'f18' or Sensor_id eq 'f19' or Sensor_id eq 'amsr2' or Sensor_id eq 'gmi') then $
      isConical=1
   if (Sensor_id eq 'atms' or Sensor_id eq 'saphir') then isCrosstrack=1

   IF (Sensor_id eq 'saphir' and Param eq 'clw') THEN BEGIN
       print, 'CLW regression not supported for sensor: '+Sensor_id
       return
   endif

   if ((Sensor_id ne 'gmi' and Sensor_id ne 'f18' and Sensor_id ne 'f19' and $
        Sensor_id ne 'saphir' ) and Param eq 'gwp') then begin
       print, 'GWP regression not supported for sensor: '+Sensor_id
       return
   endif

   if ((Sensor_id ne 'f18' and Sensor_id ne 'gmi' and Sensor_id ne 'f19' and Sensor_id ne 'amsr2') and Param eq 'em') then begin
	print, 'Emiss regression not supported for sensor: '+Sensor_id
	return
   endif	

   if ( (Sensor_id ne 'gmi' and Sensor_id ne 'f18' and Sensor_id ne 'f19' and Sensor_id ne 'amsr2') and Param eq 'em2') then begin
	print, 'Emiss single channel+tsk regression not supported for sensor: '+Sensor_id
	return
   endif	

     ;---Assign specific arrays, scalars
   nProfiles=n_elements(angle) 

   ;---Get cosine of the angle for training
   AngInRads   = fltarr(n_elements(angle))
   CosAngle    = fltarr(n_elements(angle))
   AngInRads(*) = Angle(*)*(3.14159265/180.)
   CosAngle(*)  = cos(AngInRads(*))


   ;---Read Coeff File Header
   FOR iFile=0,nAlgors-1 DO BEGIN
       err=0
       File2Read=Coeff_Path+AlgorLabels(iFile)+'_regressCoeffs_'+Sensor_id+'_'+Param+'.dat'
print,File2Read
       if (Sensor_id eq 'atms') then $
         File2Read=Coeff_Path+AlgorLabels(iFile)+'_regressCoeffs_npp_'+Param+'.dat'
       openr,iu,File2Read,/get_lun,error=err
       IF (err ne 0) THEN BEGIN
           print ,!ERROR_STATE.MSG
           STOP
       ENDIF
       ;---Header
       readf,iu,format='(60x,i4)',nLay
       readf,iu,format='(60x,i4)',nLev
       readf,iu,format='(60x,i4)',nchan
       readf,iu,format='(60x,i4)',nAngleBins
       AngleBins=intarr(nAngleBins+1)
       readf,iu,format='(60x,20i4)',AngleBins
       cfreq   = fltarr(nchan)
       pol     = intarr(nchan)
       presLay = fltarr(nLay)
       presLev = fltarr(nLev)
       readf,iu,format='(60x,100f12.4)',cfreq
       readf,iu,format='(60x,100i3)',pol
       readf,iu,format='(60x,120f9.3)',presLay
       readf,iu,format='(60x,120f9.3)',presLev
       
       if (iFile eq 0) then begin
           Algors      = fltarr(nAlgors,nPar,mxIndepVar,nAngleBins)
           Labels      = ''
           nElemts     = 0
           nIndepVar   = intarr(nAlgors)
           TbIndxArr   = make_array(nAlgors,mxIndepVar,/integer,value=-999)
           FormIndxArr = make_array(nAlgors,mxIndepVar,/integer,value=0)
           IndepVars   = make_array(mxIndepVar, /float, value=-99.)
       endif

       ;---Read main body of coefficient file
       print,'Regress coeff file to read: ',File2Read
       print,File2Read,iu,iSfcTyp,Labels,nElemts,nIndepVar,iSfcTyp[iFile],$
         nchan,nAngleBins,isCrossTrack
       readRegressAlgor,File2Read,iu,iSfcTyp,Algors,Labels,nElemts,nIndepVar,TbIndxArr,FormIndxArr,iSfcTyp[iFile],$
         nchan,nAngleBins,isCrossTrack
       nchan2UseCLW[isfcTyp[iFile]]=n_elements(where(TbIndxArr[isfcTyp[iFile],*] ge 0))
       close,iu
       free_lun,iu
       
   ENDFOR
   print, nchan2useclw,nindepvar

   ;---Declare arrays to store retrievals in 
   print, 'Performing retrievals.........'
   nProfswAlgos=0L
   nProfswoAlgos=0L
   dummy=-999.
   clw = make_array(1,/float,value=-999.)
   
   ;---Loop over the profiles
   FOR iprof=0L,nProfiles-1 DO BEGIN
       ;----Apply the regression algors to get the EDRs
       ang2use  = CosAngle(iprof)
       ang2use  = angle(iprof)
       sPos2use = ScanPos(iprof)
       lat2use  = lat(iprof)
       ;tb2use   = tb[*,iprof]
       tb2use   = reform(tb[iprof, *])
       sfc2use  = SfcTyp(iprof)
       clw[0]=-999.
       IF (sfc2use ne 0) THEN BEGIN
           edr(iprof)=0
           CONTINUE
       ENDIF
       iAngBin  = -999
       FOR iAng=0,nAngleBins-1 DO BEGIN
           ;---Crosstrack
           IF (ang2use ge AngleBins(iAng) and ang2use lt AngleBins(iAng+1) and ( isCrosstrack ge 1 )) THEN iAngBin = iAng
;           print,ang2use,anglebins(iang),anglebins(iang+1),iang

           IF (ang2use lt AngleBins[0] and ( isCrosstrack ge 1 )) THEN iAngBin=0
           ;---Conical
           IF (isConical ge 1) THEN ang2use=-99.
           IF (isConical eq 1) THEN iAngBin=0
       ENDFOR
       IF (sfc2use lt 0 or iAngBin lt 0) THEN BEGIN
;ESM0401   IF (param eq 'em') THEN edr(iprof,*)    = -999. ELSE edr[iprof] = -999.
;          edr[iprof] = -999.
           nProfswoAlgos = nProfswoAlgos+1
           CONTINUE
       ENDIF
       ;---Set sensor specific Independent variables
       IF (sensor_id eq 'amsr2') THEN BEGIN
          IF (param eq 'clw') THEN BEGIN
             IndepVars[0]=alog(tb2use[6]-tb2use[7])                                              
             IndepVars[1]=alog(tb2use[10]-tb2use[11])  
          ENDIF
          IF (param eq 'gwp') THEN BEGIN
              IndepVars[0]=300-alog(tb2use[13])                                              
          ENDIF
          IF (param eq 'em2') THEN BEGIN
              IndepVars[0]=tskin[iprof]
          ENDIF
       ENDIF
       IF (sensor_id eq 'f18' or sensor_id eq 'f19') THEN BEGIN
           if (param eq 'clw') THEN BEGIN
               IndepVars[0]=alog(tb2use[12]-tb2use[11])
               IndepVars[1]=alog(tb2use[15]-tb2use[14])
           endif
           if (param eq 'gwp') THEN BEGIN
                IndepVars[0]=300.-alog(tb2use[9])
           endif
           if (param eq 'em2') THEN BEGIN
               IndepVars[0]=tskin[iprof]
           endif
       ENDIF
       IF (sensor_id eq 'gmi') THEN BEGIN
           if (param eq 'clw') THEN BEGIN
               IndepVars[0]=alog(tb2use[2]-tb2use[3])
               IndepVars[1]=alog(tb2use[5]-tb2use[6])
           endif
           if (param eq 'gwp') THEN BEGIN
               IndepVars[0]=300.-alog(tb2use[9])
               IndepVars[1]=300.-alog(tb2use[11])
           endif
           if (param eq 'em2') THEN BEGIN
               IndepVars[0]=tskin[iprof]
           endif

       ENDIF
       IF (sensor_id eq 'atms') THEN BEGIN
           if (param eq 'clw') THEN BEGIN
               IndepVars[0]=alog(300.-tb2use[0])
               IndepVars[1]=alog(300.-tb2use[1])
               IndepVars[2]=alog(300.-tb2use[15])
               IndepVars[3]=lat2use
           endif
       ENDIF
       CASE param OF 
          'em': BEGIN
             emis= make_array(nElemts,/FLOAT,VALUE=-999.)
             IF (sfc2use eq 0 and iAngBin ge 0) THEN BEGIN
                ApplyRegr,isCrossTrack,nchan2UseCLW[sfc2use],nAlgors,tb2use,IndepVars,iAngBin,Algors,$
                          nElemts,nIndepVar,TbIndxArr,FormIndxArr,sfc2use,param,emis
                nProfswAlgos = nProfswAlgos+1
                edr[iprof,0:nElemts-1] = emis
             ENDIF
;             IF (edr(iprof) lt 0) THEN edr(iprof)=0
             ;---trap negative emissivities
             idx = WHERE(emis lt 0.,nidx)
             IF (nidx gt 0) THEN edr[iprof,idx] = 0.
             ;---line in kevin's 
             ;IF ((clw lt 0 or clw gt 0) and (param eq 'em' or param eq 'em2')) THEN edr(iprof)=99.
          END
          'em2': BEGIN
             emis= make_array(nElemts,/FLOAT,VALUE=-999.)
             IF (sfc2use eq 0 and iAngBin ge 0) THEN BEGIN
                ApplyRegr,isCrossTrack,nchan2UseCLW[sfc2use],nAlgors,tb2use,IndepVars,iAngBin,Algors,$
                          nElemts,nIndepVar,TbIndxArr,FormIndxArr,sfc2use,param,emis
                nProfswAlgos = nProfswAlgos+1
                edr[iprof,0:nElemts-1] = emis
             ENDIF
;             IF (edr(iprof) lt 0) THEN edr(iprof)=0
             ;---trap negative emissivities
             idx = WHERE(emis lt 0.,nidx)
             IF (nidx gt 0) THEN edr[iprof,idx] = 0.
             ;---line in kevin's 
             ;IF ((clw lt 0 or clw gt 0) and (param eq 'em' or param eq 'em2')) THEN edr(iprof)=99.
          END
          ELSE: BEGIN
            IF (sfc2use eq 0 and iAngBin ge 0) THEN BEGIN
               ApplyRegr,isCrossTrack,nchan2UseCLW[sfc2use],nAlgors,tb2use,IndepVars,iAngBin,Algors,$
                         nElemts,nIndepVar,TbIndxArr,FormIndxArr,sfc2use,param,clw
               nProfswAlgos = nProfswAlgos+1
                                ;edr(iprof) = exp(clwlog)
               edr[iprof] = clw


                                ;if (param eq 'clw') then print,'CLW',clw
                                ;if (param eq 'gwp') then print,'GWP',clw,exp(clw)
                                ;if (param eq 'gwp' and sensor_id eq 'f18') then edr[iprof]=exp(clw)
            ENDIF
            IF (edr(iprof) lt 0) THEN edr(iprof)=0
         END
         ENDCASE
   ENDFOR
   print, 'Number of profiles with regression retrieved...',strcompress(string(nProfswAlgos)+' /'+string(nProfiles))
END

;===================================================================
;
; Name: ApplyRegr
;
; Language: IDL
;
; Programming Unit: Procedure
;
; Subroutines called:
;   - None
;
; Description:
;   - This IDL Procedure applies the proper regression coefficients
;     to retrieve various geophysical parameters and returns the value
;
; Arguments/Type/Description
;     isCrossTrack  / Input    / Flag to specify if instrument is crosstrack scanner
;     nchan         / Input    / Number of channels used for regression
;     nAlgors       / Input    / Number of algorithms (usually nsurface types)
;     tb            / Input    / Array of brightness temperatures
;     angle         / Input    / cos of angle
;     AngleBin      / Input    / Index of angle bin
;     lat           / Input    / latitude of observation
;     clw           / Input    / cloud amount (if parameter uses it as predictor)
;     Algors        / Input    / Array of coefficients (nAlgors,nElts,nIndepVar,nAngleBins)
;     nElts         / Input    / Index of parameter layer
;     nIndepVar     / Input    / Number of independent variables trained on
;     TbIndxArr     / Input    / Index of channels used for training
;     FormTbIndxArr / Input    / Form of Tb used in training (0-physical, 1-log space)
;     sfcTyp        / Input    / Index of surface type
;     param         ; Input    / String of EDR parameter being retrieved
;     EDR           / Output   / Output EDR value
;
;
; Author:
;   - Kevin Garrett, RTi @ NOAA/NESDIS/STAR/JCSDA
;
; History:
;   - 4/25/2014: Initial code
;   - 4/01/2015: KG/ESM consolidated changes 
;                for emissivity regressions (single channel + multi channel)
;
;====================================================================

PRO ApplyRegr,isCrossTrack,nchan,nAlgors,tb,IndepVars,AngleBin,Algors,nElemts,$
              nIndepVar,TbIndxArr,FormTbIndxArr,sfcTyp,param,EDR

  EDR=fltarr(nElemts)
  nInd=nIndepVar(sfcTyp)
  TbIndx=reform(TbIndxArr(sfcTyp,0:nchan-1),nchan)
  FormIndx=reform(FormTbIndxArr(sfcTyp,0:nchan-1),nchan)
  FOR ivar=0,nElemts-1 DO BEGIN
      a0=Algors(sfcTyp,ivar,0,AngleBin)
      EDR(ivar)=a0

;     print , 'ivar=',ivar, ', iInd=a0',', EDR(ivar)=',EDR(ivar)
      IF (EDR(ivar) eq -99.00) THEN BEGIN
          EDR(ivar)=-999.
          break                 ; ESM 150413
      ENDIF ELSE BEGIN
          ;---Loop over spectral indep. vars 
          i=0

          IF (param ne 'em2') THEN BEGIN
              IF (nChan gt 0) THEN BEGIN
                  FOR iInd=0,nchan-1 DO BEGIN
 ;                     print , 'tb  =',tb(TbIndx(iInd)-1), ', Algors=',Algors(sfcTyp,ivar,i+1,AngleBin)
                      EDR(ivar)=EDR(ivar)+tb(TbIndx(iInd)-1)*Algors(sfcTyp,ivar,i+1,AngleBin)
  ;                    print , 'ivar=',ivar, ', iInd=',iInd, ', EDR(ivar)=',EDR(ivar)
                      i=i+1
                  ENDFOR
              ENDIF
              ;---weight by non-spectral indep. vars (angle,lat,other)
              IF (nInd gt nChan) THEN BEGIN
                  FOR iInd=0,nInd-nChan-1 DO BEGIN
                      EDR(ivar)=EDR(ivar)+IndepVars[iInd]*Algors(sfcTyp,ivar,i+1,AngleBin)
   ;                   print, anglebin,IndepVars[iInd],Algors(sfcTyp,ivar,i+1,AngleBin),IndepVars[iInd]*Algors(sfcTyp,ivar,i+1,AngleBin)
                      i=i+1
                  ENDFOR

              ENDIF

          ENDIF ELSE BEGIN  ; 'em2' - label = single channel emissivity - channel + tskin predictor
              EDR(ivar)=EDR(ivar)+tb(TbIndx(ivar)-1)*Algors(sfcTyp,ivar,1,AngleBin)
              EDR(ivar)=EDR(ivar)+IndepVars[0]*Algors(sfcTyp,ivar,2,AngleBin)
          ENDELSE
          ;; IF (nChan gt 0) THEN BEGIN
          ;;     FOR iInd=0,nchan-1 DO BEGIN
          ;;         EDR(ivar)=EDR(ivar)+tb(TbIndx(iInd)-1)*Algors(sfcTyp,ivar,i+1,AngleBin)
          ;;         i=i+1
          ;;     ENDFOR
          ;; ENDIF
          ;; ;---weight by non-spectral indep. vars (angle,lat,other)
          ;; IF (nInd gt nChan) THEN BEGIN
          ;;     FOR iInd=0,nInd-nChan-1 DO BEGIN
          ;;         EDR(ivar)=EDR(ivar)+IndepVars[iInd]*Algors(sfcTyp,ivar,i+1,AngleBin)
          ;;         i=i+1
          ;;     ENDFOR
          ;; ENDIF
      ENDELSE
  ENDFOR 
END
